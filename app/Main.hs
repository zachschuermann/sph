module Main where
import Display (display, idle)
import Lib
import Graphics.UI.GLUT
import Linear.V2
import System.Exit
import Data.IORef

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered] --, RGBMode, WithDepthBuffer]
  initialWindowSize $= Size 800 600
  _window <- createWindow "SPH"
  reshapeCallback $= Just reshape
  init_

  ps <- newIORef $ makeParticles [(100, 100), (400, 600)]
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

makeParticles :: [(Double, Double)] -> [Particle]
makeParticles pts = map maker pts
  where maker :: (Double, Double) -> Particle
        maker (x, y) = Particle (V2 x y) (V2 0 0) (V2 0 0) 0 0
