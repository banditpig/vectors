{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
module Dynamics where
import Text.Printf
import Vectors as V
import Graphics.Gloss 
import Views

type Time = Float
type Displacement = V.Vector
type Velocity = V.Vector

type State = (Time, Displacement, Velocity) 
type Accnf = State -> V.Vector

displacement :: State -> Point
displacement (_, V (x, y, _) , V (_, _, _)) = (x, y)

visualPathFromStates :: [State] -> Path
visualPathFromStates  = map displacement 

velocity :: State -> Point
velocity (_, V (_, _, _) , V (velx, vely, _)) = (velx, vely)

visualVelFromStates :: [State] -> Path
visualVelFromStates  = map velocity 

step :: Accnf -> Float -> State -> State
step f dt st@(t, r, v) = (t', r', v') where
    t' = t + dt             
    r' = r ^+^  v ^* dt      
    v' = v ^+^  f st ^* dt   

heightPositive :: State -> Bool
heightPositive (_, V(_, y, _) , _) = y > 0

solution :: Accnf -> Float -> State -> [State]
solution a dt  = iterate (step a dt) 

-- simple formating for States
displayStr :: [State] -> String
displayStr   = foldr f ""  where 
    f (t, p, v) str = str ++ "T:" ++ (printf "%.4f" t :: String) ++ " P:" ++ show p ++ " V:" ++ show v ++ "\n"

-- Projectile fire at angle theta with velocity v has x, y componenst v cos and v sin
projectileInit :: Float -> Float -> V.Vector
projectileInit v theta = V (v * cos (theta * 0.0174533), v * sin (theta * 0.0174533), 0)

-- Only accn. due to gravity and its acting down
projectile :: Accnf
projectile (_, _, _) = V (0, -9.8, 0)


-- Projectile at velocity v, angle theta. Keep evaluating while the projectile is above ground
projectileAtVandTheta :: Float -> Float -> [State]
projectileAtVandTheta v theta = takeWhile  heightPositive  $ solution projectile 0.01 (0, p0, projInit) where
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

plotSeveralProjectiles :: Float -> Float -> Float -> IO ()
plotSeveralProjectiles v theta dtheta = drawPics . pathPlots .  paths $  severalProjectiles v theta dtheta 
 
dyna :: IO ()
dyna = plotSeveralProjectiles 50 90 5



