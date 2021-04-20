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
    | a < 'A' = a
    | a <= 'Z' = if ord a < 88 then chr (ord a + 3)  else chr (ord a - 23)
    | a <= 'z' = if ord a < 120 then chr (ord a + 3) else chr (ord a - 23)
    | otherwise = a

shift ::Int -> Char -> Char
shift n a
    | (a < 'A') || (a > 'z') || ((a > 'Z') && (a < 'a')) = a
    | a <= 'Z' = if ord a < 91 - (n `mod` 26) then chr (ord a + (n `mod` 26))  else chr (ord a - (26 - n) `mod` 26)
    | a <= 'z' = if ord a < 123 - (n `mod` 26) then chr (ord a + (n `mod` 26)) else chr (ord a - (26 - n) `mod` 26)

prop_code :: Char -> Bool
prop_code a = code a == shift 3 a

prop_code_inverse :: Char -> Int -> Property
prop_code_inverse a n = (n >= 0) ==> shift (26 - n) (shift n a) == a

--QuickCheck
return []
check = $quickCheckAll
