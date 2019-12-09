module Solver (update) where
import Lib
import Graphics.UI.GLUT
import Data.IORef

dt = 0.0008

-- data Point = !GLfloat !GLfloat

-- update :: [(GLfloat, GLfloat)] -> [(GLfloat, GLfloat)]
update :: [Particle] -> [Particle]
-- update all@((Point x y) _ _ _ _ = map (\(x, y) -> (x + 0.01, y)) all
update ps = map incr ps
  where
    incr :: Particle -> Particle
    incr (Particle p v f d pr) = Particle (pp p) v f d pr
      where pp (GLPoint x y) = GLPoint (x + 0.01) y

solve :: IO ()
solve = putStrLn "test"

integrate :: [(GLfloat, GLfloat)] -> [(GLfloat, GLfloat)]
integrate [] = []
-- integrate all@(p:ps) = integrate_ p : integrate ps
--   where integrate_ p =
--           |
--             where pv = DT *
