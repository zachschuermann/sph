module Main where
import Display (display, idle)
import Lib
import Solver (update)
import Graphics.UI.GLUT
import Linear.V2
import Control.DeepSeq
import System.Environment (getArgs, getProgName)
import System.Exit
import System.Random
import Data.IORef

-- default number of points/iterations
num_points :: Int
iter :: Int
num_points = 500
iter = 1000

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- need more elegant way of doing this..
    [] -> guiMain num_points iter
    ["-t"] -> cliMain num_points iter
    ["-t", "-n", n] -> cliMain (read n) iter
    ["-t", "-n", n, "-i", iters] -> cliMain (read n) (read iters)
    ["-t", "-i", iters] -> cliMain num_points (read iters)
    _ -> usage

usage :: IO ()
usage = do pn <- getProgName
           die $ "Usage: " ++ pn ++ " [-t] [-n <number particles>] [-i <number iterations>]"

cliMain :: Int -> Int -> IO ()
cliMain nps iters = do
  particles <- simInit nps iters
  let sol = iterate update particles !! iters
  (sol !! 10) `deepseq` putStrLn "Done."

guiMain :: Int -> Int -> IO ()
guiMain nps iters = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered] -- unused: RGBMode, WithDepthBuffer
  initialWindowSize $= Size (fromIntegral window_width) (fromIntegral window_height)
  _window <- createWindow "SPH"
  reshapeCallback $= Just reshape
  init_

  particles <- simInit nps iters
  ps <- newIORef $ particles
  iterRef <- newIORef $ (0 :: Int)
 
  displayCallback $= display ps
  idleCallback $= Just (idle iters ps iterRef)
  keyboardMouseCallback $= Just (keyboard)
  mainLoop

simInit :: Int -> Int -> IO [Particle]
simInit nps iters = do
  gen <- getStdGen

  let jitters = randoms gen :: [Double]
      points = take nps $ initPoints jitters

  putStrLn $ "Starting " ++ show nps ++ " point simulation, " ++ show iters ++ " iterations..."
  return $ makeParticles points

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
 
init_ :: IO ()
init_ = do
  -- add "exit" menu entry
  attachMenu RightButton (Menu [MenuEntry "Exit" (exitWith ExitSuccess)])

keyboard :: KeyboardMouseCallback
keyboard key keyState _ _ = do
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
