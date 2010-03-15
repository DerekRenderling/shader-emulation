module Main where
import Graphics.UI.Simulation3D.Shader
import Graphics.UI.GLUT
import Control.Applicative ((<$>),(<*>))

main = do
    display = do
        prog <- simShader <$> getSimulation
        mat <- cameraMatrix <$> getCamera
        
        let [x,y,z] = take 3 $ map (!! 3) $ toLists $ inv mat
        liftIO $ withProgram prog $ preservingMatrix $ do
            bindProgram prog "C" $ vertex3f x y z
            renderObject Solid $ Sphere' 10 6 6
    
    begin = do
        ptr <- liftIO $ newArray $ take (19 * 19)
            $ cycle [ color4f 0 0 0 0.5, color4f 1 1 1 0.5 ]
        prog <- liftIO $ newProgram vertexShader fragmentShader
        
        cam <- getCamera
        setCamera $ cam {
            cameraMatrix = rotate (AxisX $ - pi / 2)
                $ translation (3 |> [0,3,0])
        }
        
        setWindowBG $ color4cf 0.8 0.8 1 1
        setSimulation $ EllipsoidSim {
            simShader = prog,
            checkerTex = PixelData RGBA Float ptr
        }
    
    onKeyDown (Char ' ') = do
        mat <- cameraMatrix <$> getCamera
        let [x,y,z] = take 3 $ map (!! 3) $ toLists $ inv mat
        liftIO $ print (x,y,z)
    onKeyDown _ = return ()
