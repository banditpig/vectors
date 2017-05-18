{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
module Views where
import Graphics.Gloss   
import Vectors as V
import Data.Monoid


lineVector :: V.Vector -> V.Vector -> Picture
lineVector  v1@(V (xc, yc, _)) v2@(V (x, y, _)) = Line [(xc, yc),  (x, y)]  <> arrowHead v1 v2

lineVectorO ::  V.Vector -> Picture
lineVectorO = lineVector V.origin

arrowHead :: V.Vector -> V.Vector -> Picture 
arrowHead v1 v2 = polygonFromVectors [v2, v2 ^+^ v', v2 ^+^ v''] where 
    v   = normalise (v1 ^-^ v2) 
    v'  = 10 *^ rotateXY (pi/8) v
    v'' = 10 *^ rotateXY ( (-1)*pi/8) v

polygonFromVectors :: [V.Vector] -> Picture
polygonFromVectors vs = Polygon pts where
    pts = map f vs where
        f (V (x, y, _) ) = (x, y)


window :: Display
window = InWindow "Window" (1400, 800) (10, 10)

background :: Color
background = white

axes :: Picture
axes = color red (line [ (-10000, 0), (10000,  0) ]) <>
       color red (line [ (0, -10000), (0,  10000) ])
 
drawIt :: [Picture] -> IO ()
drawIt ps = display window background (axes <> mconcat ps) 

