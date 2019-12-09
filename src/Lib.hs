module Lib (Particle (..)) where
import Graphics.UI.GLUT
import Linear.V2

data Particle = Particle {point :: V2 Double,
                          velocity :: V2 Double,
                          force :: V2 Double,
                          density :: Double,
                          pressure :: Double} deriving Eq
