module Main where
import Display (display, idle)
import Lib
import Graphics.UI.GLUT
import System.Exit
import Data.IORef

-- very helpful: https://wiki.haskell.org/OpenGLTutorial2

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered] --, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 800 600
  _window <- createWindow "SPH"

  init_

  -- ps <- newIORef [(0.0, 0.0), (0.3, 0.0)]
  -- ps <- makeParticles [(0.0, 0.0), (0.3, 0.0)]
  ps <- newIORef [Particle (GLPoint 0.0 0.0) (Vector 1.0 1.0) (Vector 1.0 1.0) 10.1 11.1,
                  Particle (GLPoint 0.0 0.2) (Vector 1.0 1.0) (Vector 1.0 1.0) 10.1 11.1]
  displayCallback $= display ps
  idleCallback $= Just (idle ps)
  keyboardMouseCallback $= Just (keyboard)
  mainLoop

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

