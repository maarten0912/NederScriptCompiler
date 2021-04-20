{-# LANGUAGE TemplateHaskell #-}
module Exercises1 where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Char

--1-FP.1
f :: Int -> Int
f x = 2 * x * x + 3 * x - 5

--1-FP.2
total1 :: Int -> Int
total1 0 = 0
total1 n = total1 (n-1) + n

total2 :: Int -> Int
total2 n = (n * (n+1)) `div` 3 --changed 2 to 3

prop_total n = (n >= 0) ==> total1 n == total2 n

--1-FP.3
prop_com_plus :: Int -> Int -> Bool
prop_com_plus a b = a + b == b + a

prop_com_minus :: Int -> Int -> Bool
prop_com_minus a b = a - b == b - a

--1-FP.4
code :: Char -> Char
code a
    | (a < 'A') || (a > 'z') || ((a > 'Z') && (a < 'a')) = a
    | a <= 'Z' = if ord a < 88 then chr (ord a + 3)  else chr (ord a - 23)
    | a <= 'z' = if ord a < 120 then chr (ord a + 3) else chr (ord a - 23)

shift ::Int -> Char -> Char
shift n a
    | (a < 'A') || (a > 'z') || ((a > 'Z') && (a < 'a')) = a
    | a <= 'Z' = if ord a < 91 - (n `mod` 26) then chr (ord a + (n `mod` 26))  else chr (ord a - (26 - n) `mod` 26)
    | a <= 'z' = if ord a < 123 - (n `mod` 26) then chr (ord a + (n `mod` 26)) else chr (ord a - (26 - n) `mod` 26)

prop_code :: Char -> Bool
prop_code a = code a == shift 3 a

prop_code_inverse :: Char -> Int -> Property
prop_code_inverse a n = (n >= 0) ==> shift (26 - n) (shift n a) == a

--1.FP.5
interest :: Double -> Double -> Double -> Double
interest a 1 r = a * (100 + r) / 100
interest a n r = (interest a (n - 1) r) * (100 + r) / 100

--1.FP.6

discr :: Double -> Double -> Double -> Double
discr a b c = b * b - 4 * a * c

root1 :: Double -> Double -> Double -> Double
root1 a b c
    | discr a b c >= 0 = (-b + sqrt (discr a b c)) / (2 * a)
    | otherwise = error "negative discriminant"

root2 :: Double -> Double -> Double -> Double
root2 a b c
    | discr a b c >= 0 = (-b - sqrt (discr a b c)) / (2 * a)
    | otherwise = error "negative discriminant"

--1.FP.7
g :: Double -> Double -> Double -> Double -> Double
g a b c x = a * x * x + b * x + c

extrX :: Double -> Double -> Double -> Double
extrX a b c = -b / (2 * a)

extrY :: Double -> Double -> Double -> Double
extrY a b c = g a b c (extrX a b c)

--1.FP.8
mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = mylength xs + 1

prop_mylength :: [a] -> Bool
prop_mylength xs = mylength xs == length xs

mysum :: [Int] -> Int
mysum [] = 0
mysum (x:xs) = x + mysum xs

prop_mysum :: [Int] -> Bool
prop_mysum xs = mysum xs == sum xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs : x

-- prop_myreverse :: [a] -> Bool
-- prop_myreverse xs = myreverse xs == reverse xs

--QuickCheck
return []
check = $quickCheckAll