module Shader (
    Sources(..),
    newProgram, newFromFiles, withProgram, bindProgram, bindvProgram
) where
import Graphics.UI.GLUT
import Control.Monad (when,unless)
import Data.Maybe (isJust,fromJust)
import Foreign (Ptr)
import qualified Language.Preprocessor.Cpphs as C

data Sources = Sources {
    vFile :: FilePath,
    fFile :: FilePath,
    sourceOpts :: C.BoolOptions,
    sourceSearch :: [FilePath], -- search paths for #include
    sourceDefs :: [(String,String)] -- pre-defined values
}

-- create a program from sources and options
newProgram :: Sources -> IO Program
newProgram sources = do
    vShader <- compile (vFile sources) sources
    fShader <- compile (fFile sources) sources
    
    [prog] <- genObjectNames 1
    attachedShaders prog $= ([vShader], [fShader])
    linkProgram prog
    reportErrors
    ok <- get $ linkStatus prog
    unless ok $ do
        putStrLn =<< (get $ programInfoLog prog)
        exitApp "Shader failed to compile"
    return prog

-- create a program from vertex and fragment files and default options
newFromFiles :: FilePath -> FilePath -> IO Program
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
compile :: Shader s => FilePath -> Sources -> IO s
compile srcFile sources = do
    [shader] <- genObjectNames 1
    (shaderSource shader $=) . (:[])
        =<< C.macroPass [] (sourceOpts sources)
        =<< C.cppIfdef srcFile
            [] [".","./glsl"] (sourceOpts sources)
        =<< readFile srcFile
    
    compileShader shader
    reportErrors
    ok <- get $ compileStatus shader
    unless ok $ do
        putStrLn =<< (get $ shaderInfoLog shader)
        exitApp "Shader failed to compile"
    return shader

-- exit opengl app (hidden)
exitApp :: String -> IO ()
exitApp msg = do
    leaveMainLoop
    win <- get currentWindow
    when (isJust win) $ destroyWindow (fromJust win)
    fail msg
