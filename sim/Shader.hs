{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Shader (
    Sources(..), Prog(..),
    newGPU, newGPU', newCPU,
    withProgram, bindProgram, bindvProgram,
    withTexture2D, preprocess
) where
import Graphics.UI.GLUT
import Data.Maybe (isJust,fromJust)
import Foreign (Ptr,mallocBytes,peekArray)
import qualified Language.Preprocessor.Cpphs as C
import Control.Applicative ((<$>))
import Control.Monad.Error (ErrorT,throwError,liftIO)
import Control.Monad (forM_)
import Data.List (intersperse)

import System.Cmd.Utils (PipeHandle,hPipeBoth)
import System.IO (Handle,hPrint,hPutStrLn,hGetBuf,hFlush)

data Sources = Sources {
    vFile :: FilePath,
    fFile :: FilePath,
    sourceOpts :: C.BoolOptions,
    sourceSearch :: [FilePath], -- search paths for #include
    sourceDefs :: [(String,String)] -- pre-defined values
}

type CPUProgram = (PipeHandle, Handle, Handle)

data Prog
    = GPU { gpuProg :: Program }
    | CPU { cpuProg :: CPUProgram }

instance Show Prog where
    show GPU{} = "GPU"
    show CPU{} = "CPU"

class Show a => CpuUniform a where
    cpuShow :: a -> String
    cpuShow = show

instance Show a => CpuUniform (Vertex2 a) where
    cpuShow (Vertex2 x y) = show (x,y)
instance Show a => CpuUniform (Vertex3 a) where
    cpuShow (Vertex3 x y z) = show (x,y,z)

type ShaderT = ErrorT String IO

-- | Create a gpu program from vertex and fragment files and default options
newGPU :: FilePath -> FilePath -> ShaderT Prog
newGPU v f = newGPU'
    $ Sources {
        vFile = v,
        fFile = f,
        sourceOpts = C.defaultBoolOptions { C.locations = False },
        sourceSearch = [".","./glsl"],
        sourceDefs = []
    }

-- | Create a gpu program from sources and options
newGPU' :: Sources -> ShaderT Prog
newGPU' sources = do
    vShader <- compile (vFile sources) sources
    fShader <- compile (fFile sources) sources
    
    prog <- liftIO $ do
        [prog'] <- genObjectNames 1
        attachedShaders prog' $= ([vShader], [fShader])
        linkProgram prog'
        return prog'
    
    ((liftIO $ get $ linkStatus prog) >>=) $ \ok -> if ok
        then return $ GPU prog
        else (throwError =<<)
            $ ((++ "\nFailed to link program.\n") <$>)
            $ liftIO (get $ programInfoLog prog)

-- | Create an emulated shader from a command.
-- The command should read (width,height) followed by the uniform parameters
-- line-by-line. Output should be raw RGB bytes on stdout.
newCPU :: FilePath -> [String] -> IO Prog
newCPU cmd args = do
    cpu@(_,_,fh) <- hPipeBoth cmd args
    Size w h <- get windowSize
    hPrint fh (w,h)
    hFlush fh
    return $ CPU cpu

-- | Bind uniform variables to a program object
bindProgram :: (Uniform a, Show a, CpuUniform a) => Prog -> String -> a -> IO ()
bindProgram (GPU prog) key value = do
    location <- get $ uniformLocation prog key
    reportErrors
    uniform location $= value
bindProgram CPU{ cpuProg = (_,_,fh) } key value = do
    hPutStrLn fh $ cpuShow value
    hFlush fh

-- | Bind uniform variables to a program object
bindvProgram :: Uniform a => Prog -> String -> Int -> Ptr a -> IO ()
bindvProgram (GPU prog) key size ptr = do
    location <- get $ uniformLocation prog key
    reportErrors
    uniformv location (fromIntegral size) ptr
bindvProgram CPU{} key size ptr = fail "Not supported."

-- | Run a shader over some stateful operations
withProgram :: Prog -> IO () -> IO ()
withProgram prog@(GPU gpu) f = do
    currentProgram $= Just gpu
    f
    currentProgram $= Nothing
withProgram prog@CPU{ cpuProg = (_,rh,wh) } f = do
    hFlush wh
    size@(Size w h) <- get windowSize
    ptr <- mallocBytes (fromIntegral $ 3 * w * h)
    hGetBuf rh ptr (fromIntegral $ 3 * w * h)
    withTexture2D size (PixelData RGB UnsignedByte ptr) f

withTexture2D :: Size -> PixelData (Color3 GLubyte) -> IO () -> IO ()
withTexture2D (Size w h) texData f = do
    -- save texture capability
    prevCap <- get $ texture Texture2D
    shadeModel $= Flat
    rowAlignment Unpack $= 1
    texture Texture2D $= Enabled
    
    [tex] <- genObjectNames 1
    textureBinding Texture2D $= Just tex
    textureFunction $= Decal
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    texImage2D
        Nothing -- not a cube map
        NoProxy -- standard texture 2d
        0 -- level 0
        RGB' -- internal format
        (TextureSize2D w h) -- texture size
        0 -- border
        texData
    f -- user geometry
    
    -- set texture capability back to whatever it was before
    --texture Texture2D $= prevCap

-- compile a shader from source (hidden)
compile :: Shader s => FilePath -> Sources -> ShaderT s
compile srcFile sources = do
    shader <- liftIO $ do
        [sh] <- genObjectNames 1
        (shaderSource sh $=) . (:[])
            =<< preprocess srcFile sources
        compileShader sh
        return sh
    
    ((liftIO $ get $ compileStatus shader) >>=) $ \ok -> if ok
        then return shader
        else (throwError =<<)
            $ (((++ "\nIn file: " ++ srcFile ++ "\n")) <$>)
            $ liftIO (get $ shaderInfoLog shader)

preprocess :: FilePath -> Sources -> IO String
preprocess srcFile sources =
    C.macroPass [] (sourceOpts sources)
    =<< C.cppIfdef srcFile
        [] (sourceSearch sources) (sourceOpts sources)
    =<< readFile srcFile
