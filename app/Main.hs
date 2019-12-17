module Main where
import Display (display, idle)
import Lib
import Solver (update, supdate)
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
chunks :: Int
num_points = 500
iter = 1000
chunks = 50

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- need more elegant way of doing this..
    [] -> guiMain num_points iter chunks
    ["-t"] -> cliMainPar num_points chunks iter
    ["-t", "-n", n] -> cliMainPar (read n) chunks iter
    ["-t", "-n", n, "-i", iters] -> cliMainPar (read n) chunks (read iters)
    ["-t", "-i", iters] -> cliMainPar num_points chunks (read iters)
    ["-ts"] -> cliMainSeq num_points iter
    ["-ts", "-n", n] -> cliMainSeq (read n) iter
    ["-ts", "-n", n, "-i", iters] -> cliMainSeq (read n) (read iters)
    ["-ts", "-i", iters] -> cliMainSeq num_points (read iters)
    _ -> usage
  where cliMainPar nps ncs = (cliMain $ update chunks) nps chks
          where chks = nps `div` ncs
        cliMainSeq = (cliMain supdate) chunks

usage :: IO ()
usage = do pn <- getProgName
           die $ "Usage: " ++ pn ++
             " [-ts] [-n <number particles>] [-i <number iterations>]" ++
             " [-c <number of chunks>]"


cliMain :: ([Particle] -> [Particle]) -> Int -> Int -> Int -> IO ()
cliMain f nps iters chks = do
  particles <- simInit nps iters chks
  let sol = iterate f particles !! iters
  sol `deepseq` putStrLn "Done."

-- cliMainSeq :: Int -> Int -> IO ()
-- cliMainSeq nps iters = do
--   particles <- simInit nps iters
--   let sol = iterate supdate particles !! iters
--   sol `deepseq` putStrLn "Done."

guiMain :: Int -> Int -> Int -> IO ()
guiMain nps iters chks = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered] -- unused: RGBMode, WithDepthBuffer
  initialWindowSize $= Size (fromIntegral window_width) (fromIntegral window_height)
  _window <- createWindow "SPH"
  reshapeCallback $= Just reshape
  init_

  particles <- simInit nps iters chks
  ps <- newIORef $ particles
  iterRef <- newIORef $ (0 :: Int)
 
  displayCallback $= display ps
  idleCallback $= Just (idle (num_points `div` chunks) iters ps iterRef)
  keyboardMouseCallback $= Just (keyboard)
  mainLoop

simInit :: Int -> Int -> Int -> IO [Particle]
simInit nps iters chks = do
  gen <- getStdGen

  let jitters = randoms gen :: [Double]
      points = take nps $ initPoints jitters

  putStrLn $ "Starting " ++ show nps ++ " point simulation, "
    ++ show iters ++ " iterations (" ++ show chks ++ " chunks)..."
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
