module Display (display, idle) where
import Solver (update)
import Lib
import Linear.V2
import Graphics.UI.GLUT
import Data.IORef
import System.Exit

drawPoints :: [(Float, Float)] -> IO ()
drawPoints ps = do
  let vertex3f x' y' z = vertex $ Vertex3 x' y' (z :: GLfloat)
      color3f r gr b = color $ Color3 r gr (b :: GLfloat)
  clear [ColorBuffer]
  matrixMode $= Projection
  loadIdentity
  ortho 0 view_width 0 view_height 0 1
  pointSize $= realToFrac h
  pointSmooth $= Enabled
  color3f 0.7188 0.9098 1
  renderPrimitive Points $
    mapM_ (\(x, y) -> vertex3f x y 0) ps
   
display :: IORef [Particle] -> DisplayCallback
display ps = do
  ps' <- get ps
  drawPoints (getPoints ps')
  swapBuffers

getPoints :: [Particle] -> [(Float, Float)]
getPoints ps = map getPoint ps

getPoint :: Particle -> (Float, Float)
getPoint (Particle p _ _ _ _) = parse p
  where parse (V2 x y) = (realToFrac x, realToFrac y)

idle :: Int -> IORef [Particle] -> IORef Int -> IdleCallback
idle num_iters ps iter = do
  ps' <- get ps
  i <- get iter

  if i > num_iters
    then exitWith ExitSuccess
    else writeIORef iter (i+1)

  writeIORef ps (update ps')
  postRedisplay Nothing
