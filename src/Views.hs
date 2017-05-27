{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
module Views where
import Graphics.Gloss   
import Vectors as V
import Data.Monoid


lineVector :: V.Vector -> V.Vector -> Picture
lineVector  pv@(V (xp, yc, _)) v@(V (x, y, _)) 
   = Line 
     [(xp, yc),  (x, y)]  
     <> arrowHead pv v

lineVectorO ::  V.Vector -> Picture
lineVectorO = lineVector V.origin 

arrowHead :: V.Vector -> V.Vector -> Picture 
arrowHead pv v = polygonFromVectors [v, v ^+^ v', v ^+^ v''] where 
    vdiff   = neg . normalise $ v ^-^ pv
    v'      = 10 *^ rotateXY (pi/8) vdiff
    v''     = 10 *^ rotateXY ( (-1)*pi/8) vdiff

polygonFromVectors :: [V.Vector] -> Picture
polygonFromVectors vs = Polygon pts where
    pts = map f vs where
        f (V (x, y, _) ) = (x, y)


window :: Display
window = InWindow "Window" (1400, 800) (10, 10)

background :: Color
background = greyN 0.7

axes :: Picture
axes = color red (line [ (-10000, 0), (10000,  0) ]) <>
       color red (line [ (0, -10000), (0,  10000) ])

vecsAtOrigin :: Int -> V.Vector ->  IO ()
vecsAtOrigin n = vecsAtPos n origin
   
vecsAtPos  :: Int -> V.Vector ->  V.Vector -> IO ()
vecsAtPos n p =
    drawPics 
        . take n
        . map (\x -> lineVector p (x ^+^ p))
        . iterate (V.rotateXY (2*pi / fromIntegral n) )

drawPics :: [Picture] -> IO ()
drawPics ps = display window background (axes <> mconcat ps) 

-- main :: IO ()
-- main = display window background axes 

