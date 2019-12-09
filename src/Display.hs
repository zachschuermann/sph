module Display (display, idle) where
import Graphics.UI.GLUT
import Data.IORef

drawPoints :: [(Float, Float)] -> IO ()
drawPoints ps = do
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
      vertex3f x' y' z = vertex $ Vertex3 x' y' (z :: GLfloat)
  clear [ColorBuffer]
  loadIdentity
  pointSize $= 10
  pointSmooth $= Enabled
  renderPrimitive Points $
    -- color3f 1 0 1
    mapM_ (\(x, y) -> vertex3f x y 0) ps
   
display :: IORef GLfloat -> DisplayCallback
display x = do
  x' <- get x
  drawPoints [(0+x', 0), (0.2+x', 0), (0.5+x', 0.5)]
  swapBuffers

idle :: IORef GLfloat -> IdleCallback
idle x = do
  x $~! (+ 0.01)
  postRedisplay Nothing
