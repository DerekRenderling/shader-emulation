module Main where
import Graphics.UI.GLUT
import Control.Applicative ((<$>),(<*>))
import Control.Concurrent (newMVar,takeMVar,putMVar)
import qualified Data.Set as Set
import Control.Monad (liftM2)
import Shader

data State = State {
    cameraPos :: Vertex3 GLfloat,
    cameraDir :: Vertex3 GLfloat,
    keySet :: Set.Set Key,
    mousePos :: (Int,Int),
    simProg :: Program
}

main :: IO ()
main = do
    (_, argv) <- getArgsAndInitialize
    initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
    initialWindowSize $= Size 800 600
    initialWindowPosition $= Position 0 0
    createWindow "ray tracer"
    depthMask $= Enabled
    lighting $= Disabled
    
    prog <- newProgram =<< liftM2 (,) (readFile "vert.c") (readFile "frag.c")
    stateVar <- newMVar $ State {
        cameraPos = Vertex3 0 (-5) 0,
        cameraDir = Vertex3 0 (-1) 0,
        keySet = Set.empty,
        mousePos = (0,0),
        simProg = prog
    }
    
    -- return from the main loop so ghci stays running
    actionOnWindowClose $= MainLoopReturns
    -- bind some callbacks then launch the main loop
    (keyboardMouseCallback $=) . Just $ \a b c d -> do
        state0 <- takeMVar stateVar
        state <- keyboard state0 a b c d
        putMVar stateVar state
    (displayCallback $=) $ do
        state <- display =<< takeMVar stateVar
        putMVar stateVar state
        return ()
    (reshapeCallback $=) . Just $ \size -> do
        viewport $= (Position 0 0, size)
        loadIdentity
    mainLoop

keyboard :: State -> Key -> KeyState -> Modifiers -> Position -> IO State
keyboard state key keyState modifiers pos = do
    return state

-- display callback for drawin' stuffs
display :: State -> IO State
display state = do
    clearColor $= Color4 0 0 0 1
    clear [ ColorBuffer ]
    
    loadIdentity
    let prog = simProg state
    bindProgram prog "C" (cameraPos state)
    bindProgram prog "D" (cameraDir state)
    
    withProgram prog $ renderPrimitive Quads $ do
        --color $ Color3 0.75 0.65 (0.55 :: GLfloat)
        color $ Color3 1 1 (1 :: GLfloat)
        mapM_ vertex [
                Vertex3 (-1) 1 0, Vertex3 1 1 0,
                Vertex3 1 (-1) 0, Vertex3 (-1) (-1) 0
                :: Vertex3 GLfloat
            ]
    
    flush
    swapBuffers
    postRedisplay Nothing
    
    return state
