module Display (display, idle) where
import Solver (update)
import Lib
import Linear.V2
import Graphics.UI.GLUT
import Data.IORef

view_width :: Double
view_width = 1.5*800.0
view_height :: Double
view_height = 1.5*600.0

drawPoints :: [(Float, Float)] -> IO ()
drawPoints ps = do
  let vertex3f x' y' z = vertex $ Vertex3 x' y' (z :: GLfloat)
      -- color3f r g b = color $ Color3 r g (b :: GLfloat)
  clear [ColorBuffer]
  loadIdentity
  ortho 0 view_width 0 view_height 0 1
  pointSize $= 16 -- TODO
  pointSmooth $= Enabled
  renderPrimitive Points $
    -- color3f 1 0 1
    mapM_ (\(x, y) -> vertex3f x y 0) ps
   
-- display :: IORef [(GLfloat, GLfloat)] -> DisplayCallback
display :: IORef [Particle] -> DisplayCallback
display ps = do
  ps' <- get ps
  drawPoints (getPoints ps') -- [(0+x', 0), (0.2+x', 0), (0.5+x', 0.5)]
  swapBuffers

getPoints :: [Particle] -> [(Float, Float)]
getPoints ps = map getPoint ps

getPoint :: Particle -> (Float, Float)
getPoint (Particle p _ _ _ _) = parse p
  where parse (V2 x y) = (realToFrac x, realToFrac y)

-- idle :: IORef [(GLfloat, GLfloat)] -> IdleCallback
idle :: IORef [Particle] -> IdleCallback
idle ps = do
  ps' <- get ps
  writeIORef ps (update ps')
  postRedisplay Nothing
