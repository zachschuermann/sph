module Solver (update) where
import Lib
import Linear.V2
import Linear.Metric (norm, quadrance)

g         = V2 0 (12000 * (-9.8)) -- 12000?
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
update = integrate . forces . densityPressure
-- update = forces
-- update ps = map incr ps
--   where
--     incr :: Particle -> Particle
--     incr (Particle p v f d pr) = Particle (pp p) v f d pr
--       where pp (V2 x y) = V2 (x + 1) y

integrate :: [Particle] -> [Particle]
integrate ps = map integrate_ ps
   where integrate_ (Particle p v f d pr) = Particle updateP updateV f d pr -- enforceBC $ Particle updateP updateV f d pr
           where updateP = p --p + dt * v
                 updateV = v --v + (dt * (f / (realToFrac d)))

enforceBC :: Particle -> Particle
enforceBC = bot . top . left . right
  where bot pa@(Particle p v f d pr)
          | check p = newp p v f d pr
          | otherwise  = pa
          where check (V2 px _) = px - eps > 0.0
                newp (V2 _ py) (V2 vx vy) f' d' pr' =
                  Particle (V2 eps py) (V2 (vx * bound_damping) vy) f' d' pr'
        top pa@(Particle p v f d pr)
          | check p = newp p v f d pr
          | otherwise  = pa
          where check (V2 px _) = px + eps > view_width
                newp (V2 _ py) (V2 vx vy) f' d' pr' =
                  Particle (V2 (view_width - eps) py) (V2 (vx * bound_damping) vy) f' d' pr'
        left pa@(Particle p v f d pr)
          | check p = newp p v f d pr
          | otherwise  = pa
          where check (V2 _ py) = py - eps < 0.0
                newp (V2 px _) (V2 vx vy) f' d' pr' =
                  Particle (V2 px eps) (V2 vx (vy * bound_damping)) f' d' pr'
        right pa@(Particle p v f d pr)
          | check p = newp p v f d pr
          | otherwise  = pa
          where check (V2 _ py) = py + eps > view_height
                newp (V2 px _) (V2 vx vy) f' d' pr' =
                  Particle (V2 px (view_height - eps)) (V2 vx (vy * bound_damping)) f' d' pr'

densityPressure :: [Particle] -> [Particle]
densityPressure ps = map (calcDP ps) ps

calcDP :: [Particle] -> Particle -> Particle
calcDP ps pi'@(Particle pix piv pif pid pipr) = Particle pix piv pif newpid newpipr
  where (newpid, newpipr) = folder $ map go ps
          where go :: Particle -> (Double, Double)
                go pj@(Particle pjx pjv pjf pjd pjpr)
                  | r2 < hsq  = (pid + mass * poly6 * (hsq-r2)^3, pipr')
                  | otherwise = (0, pipr')
                  where pipr' = gas_const*(pid - rest_dens)
                        r2 = quadrance (pjx - pix)
                folder = foldl (\(ax, ay) (x, y) -> (ax + x, ay + y)) (0, 0)

fGravity :: Double -> V2 Double
fGravity dens = g * (realToFrac dens)

forces :: [Particle] -> [Particle]
forces ps = map (calcPV ps) ps
-- forces ps = map (calcForce ps) ps
--   where calcForce :: [Particle] -> Particle -> Particle
--         calcForce ps pp@(Particle p v f d pr) = Particle p v newf d pr
--         where let fgrav = fGravity d in newf = fpress + fvisc + fgrav
--           where (fpress, fvisc) = sum $ map (calcPV pp) ps
--                   where calcPV :: Particle -> Particle -> [(V2 Double, V2 Double)]
--                         calcPV (Particle p' v' f' d' pr') (Particle p'' v'' f'' d'' pr'') =

calcPV :: [Particle] -> Particle -> Particle
calcPV ps pi'@(Particle pix piv pif pid pipr) = Particle pix piv (fpress + fvisc + fgrav) pid pipr
  where
    fgrav = fGravity pid
    (fpress, fvisc) = folder $ map go ps
          where go :: Particle -> (V2 Double, V2 Double)
                go pj@(Particle pjx pjv pjf pjd pjpr)
                  | pi' == pj = (V2 0 0, V2 0 0)
                  | r < h     = (fpress', fvisc')
                  | otherwise = (V2 0 0, V2 0 0)
                  where fpress' = (-(pjx - pix)/(realToFrac r)*(realToFrac mass)*(realToFrac (pipr + pjpr)) /
                                  (realToFrac (2.0 * pjd)) * realToFrac spiky_grad * (realToFrac (h-r))^2) -- TODO normallize
                        fvisc' = visc * (realToFrac mass) * (pjv - piv) /
                          (realToFrac pjd) * realToFrac visc_lap * realToFrac (h-r)
                        r = norm (pjx - pix)
                folder = foldl (\(ax, ay) (x, y) -> (ax + x, ay + y)) ((V2 0 0), (V2 0 0))
