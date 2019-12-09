module Solver (update) where
import Lib
import Graphics.UI.GLUT
import Data.IORef

g         = Vector 0 (12000 * (-9.8)) -- 12000?
rest_dens = 1000.0
gas_const = 2000.0
h         = 16.0     -- kernel radius
hsq       = h*h
mass      = 65.0     -- assume all particles have the same mass
visc      = 250.0    -- viscosity constant
dt        = 0.0008

poly6 = 315.0 / (65.0*pi*(h^9))
spiky_grad = (-45.0) / (pi*h^6)
visc_lap = 45.0 / (pi*h^6)

-- simulation parameters
eps = h -- boundary epsilon
bound_damping = (-0.5)

window_width = 800;
window_height = 600;
view_width = 1.5*800.0;
view_height = 1.5*600.0;

update :: [Particle] -> [Particle]
update ps = map incr ps
  where
    incr :: Particle -> Particle
    incr (Particle p v f d pr) = Particle (pp p) v f d pr
      where pp (GLPoint x y) = GLPoint (x + 0.01) y

solve :: IO ()
solve = putStrLn "test"

integrate :: [Particle] -> [Particle]
integrate ps = map integrate_ ps
   where integrate_ (Particle p v f d pr) = enforceBC $ Particle (updateP p) (updateV v) f d pr
           where updateP (GLPoint x y) = GLPoint x y
                 updateV (Vector vx vy) = Vector vx vy

enforceBC :: Particle -> Particle
enforceBC = id
           -- | newx - eps < 0 = Particle (updateP p) (updateV v) f d pr
           -- | newx + eps < 0 = Particle (updateP p) (updateV v) f d pr
