module Lib (Particle (..), GLPoint (..), Vector (..)) where
import Graphics.UI.GLUT

data Particle = Particle {point :: GLPoint,
                          velocity :: Vector,
                          force :: Vector,
                          density :: Double,
                          pressure :: Double} deriving Eq

data GLPoint = GLPoint !GLfloat !GLfloat deriving Eq
data Vector = Vector !Double !Double deriving Eq
