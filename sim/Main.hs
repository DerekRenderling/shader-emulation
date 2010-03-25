module Main where
import Graphics.UI.GLUT
import Control.Concurrent (newMVar,takeMVar,putMVar)
import qualified Data.Set as Set

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first,(&&&))
import Control.Monad (liftM2,join,when)
import Control.Monad.Error (runErrorT)
import Data.Maybe (fromJust,isJust)

import Shader
import Util

data State = State {
    cameraPos :: Vertex3 GLfloat,
    cameraDir :: Vertex3 GLfloat,
    keySet :: Set.Set Key,
    mousePos :: (Int,Int),
    simProg :: Maybe Program,
    simCPU :: Bool
} deriving Show

main :: IO ()
main = do
    (_, argv) <- getArgsAndInitialize
    initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
    initialWindowSize $= Size 800 600
    initialWindowPosition $= Position 0 0
    createWindow "ray tracer"
    depthMask $= Enabled
    lighting $= Disabled
    
    prog <- (runErrorT (newFromFiles "vert.c" "frag.c") >>=) $ \p -> case p of
        Left msg -> putStrLn msg >> return Nothing
        Right prog -> return $ Just prog
    
    stateVar <- newMVar $ State {
        cameraPos = Vertex3 0 (-3) 0,
        cameraDir = Vertex3 0 (-1) 0,
        keySet = Set.empty,
        mousePos = (0,0),
        simProg = prog,
        simCPU = elem "--cpu" argv
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
    (reshapeCallback $=) . Just $ \size -> do
        viewport $= (Position 0 0, size)
        loadIdentity
    mainLoop

onKeyUp :: State -> Key -> IO State

-- print the state
onKeyUp state (Char ' ') =
    print state >> return state

-- recompile the shaders
onKeyUp state (Char 'r') =
    (runErrorT (newFromFiles "vert.c" "frag.c") >>=) $ \p -> case p of
        Left msg -> putStrLn msg >> return state
        Right prog -> do
            putStrLn "Recompiled for the GPU"
            return $ state { simProg = Just prog }

-- toggle cpu mode
onKeyUp state (Char 'c') = do
    let cpu = simCPU state
    print . ("Switched to " ++) . (++ " mode")
        $ if cpu then "CPU" else "GPU"
    return $ state { simCPU = not cpu }

onKeyUp state key = return state

onKeyDown :: State -> Key -> IO State
onKeyDown state (Char '\27') =
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
navigate state = state { cameraPos = c', cameraDir = d' }
    where
        (c,d) = cameraPos &&& cameraDir $ state
        (c',d') = ($ (c,d)) $ case actions of
            [] -> id
            _ -> foldl1 (.) actions
        actions = map snd
            $ filter (\(k,_) -> Set.member k (keySet state)) [
                (Char 'w', first (plus (0,-ds,0))),
                (Char 'a', first (plus (-ds,0,0))),
                (Char 's', first (plus (0,ds,0))),
                (Char 'd', first (plus (ds,0,0))),
                (Char 'q', first (plus (0,0,ds))),
                (Char 'z', first (plus (0,0,-ds)))
            ]
        ds = 0.1
        plus (x',y',z') (Vertex3 x y z) = Vertex3 (x+x') (y+y') (z+z')

display :: State -> IO State
display state = do
    clearColor $= Color4 0 0 0 1
    clear [ ColorBuffer ]
    
    loadIdentity
    
    let mProg = simProg state
    when (isJust mProg) $ do
        let prog = fromJust mProg
        withProgram prog $ renderPrimitive Quads $ do
            bindProgram prog "C" (cameraPos state)
            color $ Color3 1 1 (1 :: GLfloat)
            mapM_ vertex [
                    Vertex3 (-1) 1 0, Vertex3 1 1 0,
                    Vertex3 1 (-1) 0, Vertex3 (-1) (-1) 0
                    :: Vertex3 GLfloat
                ]
    
    flush
    swapBuffers
    postRedisplay Nothing
    
    return $ navigate state
