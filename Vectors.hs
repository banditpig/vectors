
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module Vectors where

type Scalar = Double
type XYZ = (Scalar, Scalar, Scalar)


data Vector = V { xyz :: XYZ } deriving ( Show)

-- functions on XYZ - maybe consider haing these work of Vector rather than the content, XYZ, of a Vector?
mapXYZ :: (Scalar -> Scalar) -> XYZ -> XYZ
mapXYZ f (x, y, z) = (f x, f y, f z)

sumXYZ :: XYZ -> Scalar
sumXYZ (x, y, z) = x + y + z

zipWithXYZ :: (Scalar -> Scalar -> Scalar) -> XYZ -> XYZ -> XYZ
zipWithXYZ f (x, y, z) (x', y', z') =  (f x x', f y y', f z z')

i, j, k:: Vector
i = V (1, 0, 0)
j = V (0, 1, 0)
k = V (0, 0, 1)

vAdd :: Vector -> Vector -> Vector
vAdd (V x) (V x') = V $ zipWithXYZ (+) x x'

vSub :: Vector -> Vector -> Vector
vSub (V x) (V x') = V $ zipWithXYZ (-) x x'

sMul :: Scalar -> Vector -> Vector
sMul s (V x) = V $ mapXYZ (+s) x

sMul' ::  Vector -> Scalar -> Vector
sMul' = flip sMul

sDiv :: Scalar -> Vector -> Vector
sDiv s (V x) = V $ mapXYZ (/s) x

dot :: Vector -> Vector -> Scalar
dot (V x) (V x') = sumXYZ . zipWithXYZ (*) x $  x'

cross :: Vector -> Vector -> Vector
cross (V (u1, u2, u3)) (V (v1, v2, v3)) = V (u2*v3 - u3*v2, u3*v1 - u1*v3, u1*v2 - u2*v1)

mag :: Vector -> Scalar
mag (V x) = sqrt . sumXYZ . mapXYZ (^2) $ x

instance Monoid Vector where
    mempty = V (0, 0, 0)
    mappend = vAdd
    -- mappend (V x) (V y) = V (x + x') (y + y') (z + z')

 
main :: IO ()
main = putStrLn "Hello vector"

