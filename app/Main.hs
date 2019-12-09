module Main where
import Display (display, idle)
import Lib
import Graphics.UI.GLUT
import Linear.V2
import System.Exit
import System.Random
import Data.IORef

-- window_width :: Integer
-- window_height :: Integer
window_width = 800;
window_height = 600;

num_points :: Int
num_points = 400

h         = 16.0     -- kernel radius

eps = h -- boundary epsilon

view_width = 1.5*800.0; -- TODO change to window width/height
view_height = 1.5*600.0;


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered] --, RGBMode, WithDepthBuffer]
  initialWindowSize $= Size window_width window_height
  _window <- createWindow "SPH"
  reshapeCallback $= Just reshape
  init_

  g <- getStdGen

  let jitters = randoms g :: [Double]
      points = take num_points $ initPoints jitters

  putStrLn $ "Starting " ++ show num_points ++ " point simulation..."
  -- print points
 
  ps <- newIORef $ makeParticles points
 
  displayCallback $= display ps
  idleCallback $= Just (idle ps)
  keyboardMouseCallback $= Just (keyboard)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
 
init_ :: IO ()
init_ = do
   -- set up the only menu
   attachMenu RightButton (Menu [MenuEntry "Exit" (exitWith ExitSuccess)])
   depthFunc $= Just Less

keyboard :: KeyboardMouseCallback
keyboard key keyState _ _ = do
   -- modifiers _ $= mods
   postRedisplay Nothing
   case (key, keyState) of
      (Char 'q', Down) -> exitWith ExitSuccess
      (Char '\27', Down) -> exitWith ExitSuccess
      (_, _) -> return ()

initPoints :: [Double] -> [(Double, Double)]
initPoints jitters = zipWith (\(x, y) j -> (x+j, y)) makePoints jitters
  where makePoints :: [(Double, Double)]
        makePoints = [(x, y) | y <- [view_width/4, view_width/4 + h..view_height-eps*2],
                               x <- [eps, eps+h..view_width/2]]

makeParticles :: [(Double, Double)] -> [Particle]
makeParticles pts = map maker pts
  where maker :: (Double, Double) -> Particle
        maker (x, y) = Particle (V2 x y) (V2 0 0) (V2 0 0) 0 0
