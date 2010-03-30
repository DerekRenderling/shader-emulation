module Main where
import Graphics.UI.GLUT
import Control.Concurrent (newMVar,takeMVar,putMVar)
import qualified Data.Set as Set

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first,second,(&&&),(***))
import Control.Monad (when,forM_,join)
import Control.Monad.Error (runErrorT)
import Data.Maybe (fromJust,isJust,isNothing)

import System.Process (system)
import System.Exit (ExitCode(..))

import Shader

data State = State {
    cameraPos :: Vertex3 GLfloat,
    cameraDir :: Vertex2 GLfloat,
    keySet :: Set.Set Key,
    mousePos :: (Int,Int),
    simGPU :: Maybe Prog,
    simCPU :: Maybe Prog,
    simUseCPU :: Bool
} deriving Show

dirVector :: Vertex2 GLfloat -> Vertex3 GLfloat
dirVector (Vertex2 theta rho) =
    Vertex3
        (sin theta)
        (-1 * cos theta * cos rho)
        (sin rho)
 
main :: IO ()
main = do
    (_, argv) <- getArgsAndInitialize
    initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
    initialWindowSize $= Size 800 600
    initialWindowPosition $= Position 0 0
    createWindow "ray tracer"
    depthMask $= Enabled
    lighting $= Disabled
    
    gpu <- if case argv of { ("cpu":_) -> True; _ -> False }
        then putStrLn "Using CPU emulation" >> return Nothing
        else (runErrorT (newGPU "vert.c" "frag.c") >>=) $ \p -> case p of
            Left msg -> putStrLn msg >> return Nothing
            Right prog -> return $ Just prog
    cpu <- newCPU "./frag" []
    
    stateVar <- newMVar $ State {
        cameraPos = Vertex3 0 (-3) 0,
        cameraDir = Vertex2 0 0,
        keySet = Set.empty,
        mousePos = (0,0),
        simGPU = gpu,
        simCPU = cpu,
        simUseCPU = isNothing gpu
    }
    
    actionOnWindowClose $= MainLoopReturns
    (keyboardMouseCallback $=) . Just $ \key keyState modifiers pos -> do
        state <- takeMVar stateVar
        state' <- ($ key) 
            $ ($ keyboard state key keyState modifiers pos)
            $ case keyState of
                Up -> onKeyUp
                Down -> onKeyDown
        putMVar stateVar state'
    (displayCallback $=) $ do
        state <- display =<< takeMVar stateVar
        putMVar stateVar state
        return ()
    (reshapeCallback $=) . Just $ \size@(Size w h) -> do
        viewport $= (Position 0 0, size)
        matrixMode $= Projection
        loadIdentity
        let as = fromIntegral w / fromIntegral h
            sa = recip as
        ortho (-as) as (-1.0) 1.0 0.0 10000.0
    mainLoop

onKeyUp :: State -> Key -> IO State
-- print the state
onKeyUp state (Char ' ') = do
    print state
    print $ dirVector $ cameraDir $ state
    return state
-- recompile for the gpu
onKeyUp state@State{ simUseCPU = False } (Char 'r') =
    (runErrorT (newGPU "vert.c" "frag.c") >>=) $ \p -> case p of
        Left msg -> putStrLn msg >> return state
        Right gpu -> do
            putStrLn "Recompiled for the GPU"
            return $ state { simGPU = Just gpu }
-- recompile for the cpu
onKeyUp state@State{ simUseCPU = True } (Char 'r') = do
    (=<< system "make clean && make frag") $ \status -> case status of
        ExitSuccess -> do
            putStrLn "Recompiled for the CPU"
            case simCPU state of
                Nothing -> return ()
                Just prog -> killCPU prog
            (\cpu -> state { simCPU = cpu }) <$> newCPU "./frag" []
        ExitFailure{} -> do
            putStrLn "Failed to compile for the CPU"
            return state

-- toggle cpu mode
onKeyUp state (Char 'c') = do
    let useCPU = simUseCPU state
    putStrLn . ("Switched to " ++) . (++ " mode")
        $ if useCPU then "GPU" else "CPU"
    return $ state { simUseCPU = not useCPU }
onKeyUp state key = return state

onKeyDown :: State -> Key -> IO State
-- escape key exits application
onKeyDown state (Char '\27') = do
    case simCPU state of
        Nothing -> return ()
        Just prog -> killCPU prog
    leaveMainLoop >> return state
onKeyDown state key = return state

keyboard :: State -> Key -> KeyState -> Modifiers -> Position -> State
keyboard state key keyState modifiers pos = state'
    where
        state' = state { keySet = f key (keySet state) }
        f = case keyState of
            Up -> Set.delete
            Down -> Set.insert

navigate :: State -> State
navigate state@State{ cameraPos = c, cameraDir = d } = state'
    where
        state' = state { cameraPos = c', cameraDir = d' }
        (c',d') = ($ (c,d)) $ case actions of
            [] -> id
            _ -> foldl1 (.) actions
        actions = map snd
            $ filter (\(k,_) -> Set.member k (keySet state)) [
                (Char 'w', first $ plus3 (0,dt,0)), -- forward
                (Char 'a', first $ plus3 (dt,0,0)), -- left
                (Char 's', first $ plus3 (0,-dt,0)), -- back
                (Char 'd', first $ plus3 (-dt,0,0)), -- right
                (Char 'q', first $ plus3 (0,0,-dt)), -- up
                (Char 'z', first $ plus3 (0,0,dt)), -- down
                (Char 'h', second $ plus2 (-dr,0)), -- turn left
                (Char 'j', second $ plus2 (0,-dr)), -- turn down
                (Char 'k', second $ plus2 (0,dr)), -- turn up
                (Char 'l', second $ plus2 (dr,0)) -- turn right
            ]
        dt = 0.1
        dr = 0.1
        plus2 (x',y') (Vertex2 x y) = Vertex2 (x+x') (y+y')
        plus3 (x',y',z') (Vertex3 x y z) = Vertex3 (x+x') (y+y') (z+z')

display :: State -> IO State
display state = do
    clearColor $= Color4 0 0 0 1
    clear [ ColorBuffer ]
    
    matrixMode $= Modelview 0
    loadIdentity
    
    runShader state
    
    flush
    swapBuffers
    postRedisplay Nothing
    
    return $ navigate state

runShader :: State -> IO ()
runShader state = do
    mProg <- if simUseCPU state
        then return $ simCPU state
        else return $ simGPU state
    
    when (isJust mProg) $ do
        let prog = fromJust mProg
            (Vertex2 theta rho) = cameraDir state
        bindProgram prog "pos" $ cameraPos state
        bindProgram prog "dir"
            $ Vertex3 (sin theta) (-1 * cos theta * cos rho) (sin rho)
        withProgram prog $ renderPrimitive Quads $ do
            color $ Color3 1 1 (1 :: GLfloat)
            quadScreen

quadScreen :: IO ()
quadScreen = do
    Size w h <- get windowSize
    let
        as = fromIntegral w / fromIntegral h
        xy = [(-as,1),(as,1),(as,-1),(-as,-1)] :: [(GLfloat,GLfloat)]
    forM_ xy $ \(x,y) -> do
        let (tx,ty) = join (***) tpos (x,y)
            tpos :: GLfloat -> GLfloat
            tpos = fromIntegral . fromEnum . (> 0)
        texCoord $ TexCoord2 tx ty
        vertex $ Vertex3 x y 0
