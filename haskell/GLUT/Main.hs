module Main where

import Data.IORef
import Graphics.UI.GLUT

-- TODO: fill out the data type with any state that you need your scene
--       to hold. It gets fed to the update routine
data SceneState = SceneState { currentTime :: Int }

initGL :: IO ()
initGL = do
   getArgsAndInitialize
   initialDisplayMode   $= [ WithDepthBuffer, DoubleBuffered ]
   createWindow "Flag"
   depthFunc            $= Just Less
   clearColor           $= Color4 0 0 0 0
   light (Light 0)      $= Enabled
   lighting             $= Enabled
   lightModelAmbient    $= Color4 0.5 0.5 0.5 1
   diffuse (Light 0)    $= Color4 1 1 1 1
   blend                $= Enabled
   blendFunc            $= (SrcAlpha, OneMinusSrcAlpha)
   colorMaterial        $= Just (FrontAndBack, AmbientAndDiffuse)
   reshapeCallback      $= Just resizeScene
   return ()

resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1)
resizeScene s@(Size width height) = do
   viewport    $= (Position 0 0, s)
   matrixMode  $= Projection
   loadIdentity
   perspective 45 (w2/h2) 1 1000
   matrixMode  $= Modelview 0
 where
   w2 = half width
   h2 = half height
   half z = realToFrac z / 2

renderScene :: IORef SceneState -> IO ()
renderScene state = do
   clear [ ColorBuffer, DepthBuffer ]
   loadIdentity
   swapBuffers

updateScene :: IORef SceneState -> IO ()
updateScene state = do
   state' <- get state
   newTime' <- get elapsedTime
   let oldTime' = currentTime state'
   let dt = (fromIntegral $ newTime' - oldTime') / 1000

   -- TODO: perform any scene update logic here based on dt

   -- TODO: write any updated state into state''
   let state'' = (state' { currentTime = newTime' })
   writeIORef state state''

   renderScene state
   return ()

main :: IO ()
main = do
   currentState <- newIORef (SceneState { currentTime = 0 })
   initGL
   idleCallback         $= Just (updateScene currentState)
   displayCallback      $= (renderScene currentState)
   mainLoop

