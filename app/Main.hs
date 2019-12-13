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

num_points :: Int
num_points = 1000

iter :: Int
iter = 1000

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> guiMain
    ["-t"] -> cliMain
    (flag:arg:[]) -> do die $ "idk"
    _ -> usage

usage :: IO ()
usage = do pn <- getProgName
           die $ "Usage: " ++ pn ++ " [-t] [-n <number particles>] [-i <number iterations>]"

cliMain :: IO ()
cliMain = do
  gen <- getStdGen

  putStrLn $ "Starting " ++ show num_points ++ " point simulation, " ++ show iter ++ " iterations..."

  let jitters = randoms gen :: [Double]
      points = take num_points $ initPoints jitters
      particles = makeParticles points
      sol = iterate update particles !! iter !! 10

  sol `deepseq` putStrLn "Done."

guiMain :: IO ()
guiMain = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered] --, RGBMode, WithDepthBuffer]
  initialWindowSize $= Size (fromIntegral window_width) (fromIntegral window_height)
  _window <- createWindow "SPH"
  reshapeCallback $= Just reshape
  init_

  gen <- getStdGen

  let jitters = randoms gen :: [Double]
      points = take num_points $ initPoints jitters

  putStrLn $ "Starting " ++ show num_points ++ " point simulation..."
  -- print points
 
  ps <- newIORef $ makeParticles points
  iter <- newIORef $ (0 :: Int)
 
  displayCallback $= display ps
  idleCallback $= Just (idle ps iter)
  keyboardMouseCallback $= Just (keyboard)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
 
init_ :: IO ()
init_ = do
   -- set up the only menu
   attachMenu RightButton (Menu [MenuEntry "Exit" (exitWith ExitSuccess)])
   -- depthFunc $= Just Less

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
