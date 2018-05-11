{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}

module Dynamics where
import           Graphics.Gloss
import           Text.Printf
import           Vectors        as V
import           Views

type Time = Float
type Displacement = V.Vector
type Velocity = V.Vector

type State = (Time, Displacement, Velocity)
type Accnf = State -> V.Vector

-- get the x, y component from the Displacement vector
displacement :: State -> Point
displacement (_, V (x, y, _) , V (_, _, _)) = (x, y)

visualPathFromStates :: [State] -> Path
visualPathFromStates  = map displacement

-- get the x, y component from the Velocity vector
velocity :: State -> Point
velocity (_, V (_, _, _) , V (velx, vely, _)) = (velx, vely)

visualVelFromStates :: [State] -> Path
visualVelFromStates  = map velocity


step :: Accnf -> Float -> State -> State
step f dt st@(t, r, v) = (t', r', v') where
    t' = t + dt
    r' = r ^+^  v ^* dt
    v' = v ^+^  f st ^* dt

ecStep :: Accnf -> Float -> State -> State
ecStep f dt st@(t, r, v) = (t', r', v') where
    t' = t + dt
    r' = r ^+^  v' ^* dt
    v' = v ^+^  f st ^* dt

solutionWithStep :: ( Accnf -> Float -> State -> State )->  Accnf -> Float -> State -> [State]
solutionWithStep stp a dt = iterate (stp a dt)

-- ================================================================
compareYVal :: V.Vector -> Float -> (Float -> Float -> Bool) -> Bool
compareYVal (V (_, y, _)) d p = p y d

yNZero :: V.Vector -> Bool
yNZero v = compareYVal v 0 (>=)

heightPositive :: State -> Bool
heightPositive (_, d, _) = yNZero d

displayStates :: [State] -> String
displayStates   = foldr f ""  where
    f (t, p, v) str = str ++ "T:" ++ (printf "%.4f" t :: String) ++ " P:" ++ show p ++ " V:" ++ show v ++ "\n"


-- Projectile fire at angle theta with velocity v has x, y componenst v cos and v sin
projectileInit :: Float -> Float -> V.Vector
projectileInit v theta = V (v * cos (theta * 0.0174533), v * sin (theta * 0.0174533), 0)

-- Only accn due to gravity acting down
projectile :: Accnf
projectile (_, _, _) = V (0, -9.8, 0)

-- Projectile at velocity v, angle theta. Keep evaluating while the projectile is above ground
projectileAtVandTheta :: Float -> Float -> [State]
projectileAtVandTheta v theta = takeWhile  heightPositive  $ solutionWithStep ecStep projectile 0.01 (0, p0, projInit) where
    projInit = projectileInit v theta
    p0 = V (0, 0 ,0)

-- calculate projectile at v theta and keep reducing theta until < 0
severalProjectiles :: Float -> Float -> Float -> [[State]]
severalProjectiles v theta dtheta
    | theta > 0 = projectileAtVandTheta v theta : severalProjectiles v (theta -  dtheta) dtheta
    | otherwise = []

paths :: [[State]]  -> [Path]
paths = map visualPathFromStates

-- make a picture from each path
pathPlots :: [Path] -> [Picture]
pathPlots  = map (color blue . Line )

pathPlot :: Color -> Path -> Picture
pathPlot c = color c . Line

plotSeveralProjectiles :: Float -> Float -> Float -> IO ()
plotSeveralProjectiles v theta dtheta = drawPics . pathPlots .  paths $  severalProjectiles v theta dtheta


plotProjectiles :: IO ()
plotProjectiles = plotSeveralProjectiles 50 180 5


 -- dampedDrivenOsc :: Scalar -- damping constant
--                 -> Scalar -- drive amplitude
--                 -> Scalar -- drive frequency
--                 -> ( V.Vector)
dampedDrivenOsc :: Scalar -> Scalar -> Scalar -> Accnf
dampedDrivenOsc beta driveAmp omega (t,r,v)= (forceDamp ^+^ forceDrive ^+^ forceSpring) ^/ mass
    where
        forceDamp = ((-1)*beta) *^ v
        forceDrive = driveAmp * cos (omega * t) *^ i
        forceSpring = (-k') *^ r
        mass = 1
        k' = 1 -- spring constant

timeDistanceX :: State -> Scalar
timeDistanceX (_, V (x, _, _), _) = x

timeDistanceXS :: [State] -> Path
timeDistanceXS sts = zip [1..] (map timeDistanceX sts)

timeVelocityX :: State -> Scalar
timeVelocityX (_, _, V (vx, _, _) ) = vx

timeVelocityXS :: [State] -> Path
timeVelocityXS sts = zip [1..] (map timeVelocityX sts)

-- Harmonic oscillator where the force is -kx
hosc :: Scalar ->  (Time, Displacement, Velocity)  -> V.Vector
hosc k (_, r , _) = (-1)*k *^ r

-- F = - kx - cv
dampHosc ::  Scalar -> Scalar -> (Time, Displacement, Velocity)  -> V.Vector
dampHosc k c (_, V (x, y, z), V(vx, vy, vz)) = V ( -1.0 * k * x, 0, 0) ^-^ V ( c * vx, vy, vz)

oneHosc :: Color ->  (Accnf -> Float -> State -> State) -> Picture
oneHosc c stp = pathPlot c . timeVelocityXS . take  50000 $ solutionWithStep stp (hosc 2.0) 0.01 (0, p0, v0) where
   p0 = V (0, 0, 0)
   v0 = V (50, 0, 0)

oneDampHosc :: Color ->  (Accnf -> Float -> State -> State) -> Picture
oneDampHosc c stp = pathPlot c . timeVelocityXS . take  50000 $ solutionWithStep stp (dampHosc 10.0 1.0) 0.01 (0, p0, v0) where
   p0 = V (0, 0, 0)
   v0 = V (150, 0, 0)

spring :: IO ()
spring =  drawPics  [oneHosc red step, oneHosc blue ecStep, color black (line [ (0, 50), (50000,  50) ])]

springDamp :: IO ()
springDamp =  drawPics  [oneDampHosc red step, oneDampHosc blue ecStep, color black (line [ (0, 150), (50000,  150) ])]
