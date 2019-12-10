module Lib where
import Linear.V2

data Particle = Particle {point :: V2 Double,
                          velocity :: V2 Double,
                          force :: V2 Double,
                          density :: Double,
                          pressure :: Double} deriving (Eq, Show)

g :: V2 Double
rest_dens :: Double
gas_const :: Double
h :: Double
hsq :: Double
mass :: Double
visc :: Double
dt :: Double
poly6 :: Double
spiky_grad :: Double
visc_lap :: Double
eps :: Double
bound_damping :: Double
view_width :: Double
view_height :: Double
window_width :: Int
window_height :: Int

g         = V2 0 (12000 * (-9.8)) -- 12000?
rest_dens = 1000.0
gas_const = 2000.0
h         = 16.0     -- kernel radius
hsq       = h*h
mass      = 65.0     -- assume all particles have the same mass
visc      = 250.0    -- viscosity constant
dt        = 0.0009 -- 0.0008

poly6 = 315.0 / (65.0*pi*(h^(9 :: Int)))
spiky_grad = (-45.0) / (pi*h^(6 :: Int))
visc_lap = 45.0 / (pi*h^(6 :: Int))

-- simulation parameters
eps = h -- boundary epsilon
bound_damping = (-1) -- (-0.5)

window_width = 800;
window_height = 600;
view_width = 1.5 * fromIntegral window_width;
view_height = 1.5 * fromIntegral window_height;
