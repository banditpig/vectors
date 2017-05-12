import Test.QuickCheck
import Vectors as V

v_cross_v_is_zero :: Vector -> Bool
v_cross_v_is_zero v = mag (v V.>< v) == 0

a_cross_b_is_neg_b_cross_a :: Vector -> Vector -> Bool
a_cross_b_is_neg_b_cross_a a b = a V.>< b  == neg (b V.>< a)

-- a x (b + c) = a x b + a x c
cross_over_sum_is_sum_cross :: Vector -> Vector -> Vector -> Bool
cross_over_sum_is_sum_cross a b c = a V.>< (b V.^+^ c) == a V.>< b V.^+^ a V.>< c


-- (a + b) x c = a x c + b x c
sum_over_cross_is_sum_cross :: Vector -> Vector -> Vector -> Bool
sum_over_cross_is_sum_cross a b c = (a ^+^ b) V.>< c == a V.>< c ^+^ b V.>< c

jacobi :: Vector -> Vector -> Vector -> Bool 
jacobi a b c = a V.>< (b V.>< c) ^+^ b V.>< (c V.>< a) ^+^ c V.>< (a V.>< b) == origin

-- i j k under cross product
ij :: Bool
ij = i V.>< j == k && j V.>< i == (-1) *^ k  &&  mag (i V.>< i) == 0

jk :: Bool
jk = j V.>< k == i && k V.>< j == (-1) *^ i  &&  mag (j V.>< j) == 0

ki :: Bool
ki = k V.>< i == j && i V.>< k == (-1) *^ j  &&  mag (k V.>< k) == 0


-- Use Gen Int in here as using Gen Double causes test failures due to 
-- small errors in the Double arithmetic. F
instance Arbitrary Vector where
  arbitrary = do
    x  <- choose (-100000, 100000) :: Gen Int
    y  <- choose (-100000, 100000) :: Gen Int
    z  <- choose (-100000, 100000) :: Gen Int
    return $ V (fromIntegral x, fromIntegral y, fromIntegral z)

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 1 } ij
    quickCheckWith stdArgs { maxSuccess = 1 } jk
    quickCheckWith stdArgs { maxSuccess = 1 } ki
    quickCheck v_cross_v_is_zero
    quickCheck a_cross_b_is_neg_b_cross_a
    quickCheck cross_over_sum_is_sum_cross
    quickCheck sum_over_cross_is_sum_cross
    quickCheck jacobi










