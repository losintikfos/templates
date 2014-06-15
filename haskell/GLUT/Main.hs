module Main where

import Data.IORef
import Graphics.UI.GLUT

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
   displayCallback      $= renderScene
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

renderScene :: IO ()
renderScene = do
   clear [ ColorBuffer, DepthBuffer ]
   loadIdentity
   swapBuffers

updateScene :: IORef Int -> IO ()
updateScene oldTime = do
   newTime' <- get elapsedTime
   oldTime' <- get oldTime

   -- let dt = let dt' = (fromIntegral $ newTime' - oldTime') / 50
   --         in if dt' < 0.8 then dt' else 0.8
   let dt = (fromIntegral $ newTime' - oldTime') / 1000
   -- TODO: perform any scene update logic here based on dt

   writeIORef oldTime newTime'
   renderScene
   return ()

main :: IO ()
main = do
   oldTime <- newIORef (0 :: Int)
   initGL
   idleCallback         $= Just (updateScene oldTime)
   mainLoop

