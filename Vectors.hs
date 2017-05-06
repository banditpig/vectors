
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

module Vectors where

type Scalar = Double
type XYZ = (Scalar, Scalar, Scalar)

newtype Vector = V { xyz :: XYZ } deriving ( Show)

-- Map a function over the internal tuple
mapVec :: (Scalar -> Scalar) -> Vector -> Vector
mapVec f (V (x, y, z))  = V (f x, f y, f z)

-- Just sum the tuple entries
sumVec :: Vector -> Scalar
sumVec (V (x, y, z)) = x + y + z

-- Apply a scalar function to x,y,z components of each vector
zipWithVec :: (Scalar -> Scalar -> Scalar) -> Vector -> Vector -> Vector
zipWithVec f (V (x, y, z))  (V (x', y', z')) =   V (f x x', f y y', f z z')

--  The 'traditional' i, j, k 
i, j, k:: Vector
i = V (1, 0, 0)
j = V (0, 1, 0)
k = V (0, 0, 1)

-- These are fairly obvious.  The functions defined above
-- make these functions simpler and cleaner.

-- Just adding/subtracting  the corresponding components, 
-- so zipWithVec using (+) or (-)
vAdd :: Vector -> Vector -> Vector
vAdd = zipWithVec (+) 

vSub :: Vector -> Vector -> Vector
vSub  = zipWithVec (-)

-- Scalar then Vector - just map (*) over tuple entries
sMul :: Scalar -> Vector -> Vector
sMul s  = mapVec (*s)
-- or Vector then Scalar
sMul' ::  Vector -> Scalar -> Vector
sMul' = flip sMul

sDiv :: Scalar -> Vector -> Vector
sDiv s  = mapVec (/s) 

-- dot product
dot :: Vector -> Vector -> Scalar
dot x = sumVec . zipWithVec (*) x 

-- Cross product - is there a neater way?
cross :: Vector -> Vector -> Vector
cross (V (u1, u2, u3)) (V (v1, v2, v3)) =
   V (u2*v3 - u3*v2, u3*v1 - u1*v3, u1*v2 - u2*v1)

-- Magnitude of a vector
mag :: Vector -> Scalar
mag  = sqrt . sumVec . mapVec (^2) 


