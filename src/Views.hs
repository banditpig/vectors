{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
module Views where
import Graphics.Gloss   
import Vectors as V
import Data.Monoid


type VecFunc = V.XYZ  -> V.Vector
vf1, vf2, vf3, vf4, vf5, vf6 :: VecFunc 
vf1 (x, y, _) =  V.V ( 1/y , 1/x, 0)
vf2 (x, y, _) =  V.V ( exp x , exp y, 0)
vf3 (x, y, _) =  V.V ( x /10, y/10 , 0)
vf4 (x, y, _) =  V.V (sin y, sin x, 0)
vf5 (x, y, _) =  V.V ( x + y, 10 /( x + y) , 0)
vf6 (x, y, _) =  V.V (  y /10 ,  x /10, 0)

fieldPics :: ((Scalar, Scalar, Scalar) -> V.Vector) -> [Picture]
fieldPics vf = [ lineVector (V.V (x, y, 0)) (vf (x, y, 0)) | x <- [-10,-9.75.. 10], y <- [-10, -9.75.. 10] ]

lineVector :: V.Vector -> V.Vector -> Picture
lineVector  pv@(V (xp, yp, _)) v@(V (x, y, _)) 
   = Line 
     [(xp, yp),  (xp + x, yp + y)]  
     <> arrowHeadScaled 0.1  pv v

lineVectorO ::  V.Vector -> Picture
lineVectorO = lineVector V.origin 


arrowHeadScaled :: Float ->  V.Vector -> V.Vector -> Picture 
arrowHeadScaled s pv v = polygonFromVectors [pv ^+^ v,pv ^+^  v ^+^ v',pv ^+^  v ^+^ v''] where 
    vdiff     = neg . normalise $ v -- ^-^ pv
    v'        = s *^ rotateXY (pi/8) vdiff
    v''       = s *^ rotateXY ( (-1)*pi/8) vdiff
    
arrowHead :: V.Vector -> V.Vector -> Picture 
arrowHead = arrowHeadScaled 0.1

polygonFromVectors :: [V.Vector] -> Picture
polygonFromVectors vs = Polygon pts where
    pts = map f vs where
        f (V (x, y, _) ) = (x, y)


window :: Display
window = InWindow "Window" (1400, 800) (10, 10)

background :: Color
background = back -- greyN 0.7

axes :: Picture
axes = color red (line [ (-50000, 0), (50000,  0) ]) <>
       color red (line [ (0, -50000), (0,  50000) ])

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
back :: Color
back = makeColorI 230 204 255 255 --makeColorI 204 255  204 100

field :: IO ()
field = drawPics . fieldPics $ vf5
-- main :: IO ()fieldP-- main = display window background axes 

