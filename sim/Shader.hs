module Shader (
    Sources(..),
    newProgram, newFromFiles, withProgram, bindProgram, bindvProgram
) where
import Graphics.UI.GLUT
import Control.Monad (when,unless)
import Data.Maybe (isJust,fromJust)
import Foreign (Ptr)
import qualified Language.Preprocessor.Cpphs as C
import Control.Applicative ((<$>))
import Control.Monad.Error (ErrorT,throwError,liftIO)

data Sources = Sources {
    vFile :: FilePath,
    fFile :: FilePath,
    sourceOpts :: C.BoolOptions,
    sourceSearch :: [FilePath], -- search paths for #include
    sourceDefs :: [(String,String)] -- pre-defined values
}

type ShaderT = ErrorT String IO

-- create a program from sources and options
newProgram :: Sources -> ShaderT Program
newProgram sources = do
    vShader <- compile (vFile sources) sources
    fShader <- compile (fFile sources) sources
    
    prog <- liftIO $ do
        [prog'] <- genObjectNames 1
        attachedShaders prog' $= ([vShader], [fShader])
        linkProgram prog'
        -- reportErrors
        return prog'
    
    ((liftIO $ get $ linkStatus prog) >>=) $ \ok -> if ok
        then return prog
        else (throwError =<<)
            $ ((++ "\nFailed to link program.\n") <$>)
            $ liftIO (get $ programInfoLog prog)

-- create a program from vertex and fragment files and default options
newFromFiles :: FilePath -> FilePath -> ShaderT Program
newFromFiles = (newProgram .) . useFiles

useFiles :: FilePath -> FilePath -> Sources
useFiles v f =
    Sources {
        vFile = v,
        fFile = f,
        sourceOpts = C.defaultBoolOptions { C.locations = False },
        sourceSearch = ["."],
        sourceDefs = []
    }

-- bind uniform variables to a program object
bindProgram :: Uniform a => Program -> String -> a -> IO ()
bindProgram prog key value = do
    location <- get $ uniformLocation prog key
    reportErrors
    uniform location $= value

-- bind uniform variables to a program object
bindvProgram :: Uniform a => Program -> String -> Int -> Ptr a -> IO ()
bindvProgram prog key size ptr = do
    location <- get $ uniformLocation prog key
    reportErrors
    uniformv location (fromIntegral size) ptr

-- run a shader over some stateful operations
withProgram :: Program -> IO () -> IO ()
withProgram prog m = do
    currentProgram $= Just prog
    m
    currentProgram $= Nothing

-- compile a shader from source (hidden)
compile :: Shader s => FilePath -> Sources -> ShaderT s
compile srcFile sources = do
    shader <- liftIO $ do
        [sh] <- genObjectNames 1
        (shaderSource sh $=) . (:[])
            =<< C.macroPass [] (sourceOpts sources)
            =<< C.cppIfdef srcFile
                [] [".","./glsl"] (sourceOpts sources)
            =<< readFile srcFile
        compileShader sh
        -- reportErrors
        return sh
    
    ((liftIO $ get $ compileStatus shader) >>=) $ \ok -> if ok
        then return shader
        else (throwError =<<)
            $ (((++ "\nIn file: " ++ srcFile ++ "\n")) <$>)
            $ liftIO (get $ shaderInfoLog shader)
