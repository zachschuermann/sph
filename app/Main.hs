module Main where
import Display (display, idle)
import Solver (solve)
import Graphics.UI.GLUT
import System.Exit
import Data.IORef

-- very helpful: https://wiki.haskell.org/OpenGLTutorial2

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered] --, RGBMode, WithDepthBuffer ]
  initialWindowSize $= Size 500 500
  _window <- createWindow "SPH"

  init_

  x <- newIORef 0.0
  displayCallback $= display x
  idleCallback $= Just (idle x)
  keyboardMouseCallback $= Just (keyboard)
  mainLoop

init_ :: IO ()
init_ = do
   -- set up the only meny
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

