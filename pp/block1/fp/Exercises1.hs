{-# LANGUAGE TemplateHaskell #-}
module Exercises1 where
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function
import Data.Char
import Data.List

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

--1-FP.5
interest :: Double -> Double -> Double -> Double
interest a 1 r = a * (100 + r) / 100
interest a n r = (interest a (n - 1) r) * (100 + r) / 100

--1-FP.6

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

--1-FP.7
g :: Double -> Double -> Double -> Double -> Double
g a b c x = a * x * x + b * x + c

extrX :: Double -> Double -> Double -> Double
extrX a b c = -b / (2 * a)

extrY :: Double -> Double -> Double -> Double
extrY a b c = g a b c (extrX a b c)

--1-FP.8
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
myreverse (x:xs) = myreverse xs ++ [x]

prop_myreverse :: (Eq a) => [a] -> Bool
prop_myreverse xs = myreverse xs == reverse xs

mytake :: Int -> [a] -> [a]
mytake 0 xs = []
mytake n [] = []
mytake n (x:xs)
    | n >= 0 = x : mytake (n - 1) xs
    | otherwise = error "Non type-variable argument"

prop_mytake :: (Eq a) => Int -> [a] -> Property
prop_mytake n xs = (n >= 0) ==> mytake n xs == take n xs

myelem :: (Eq a) => a -> [a] -> Bool
myelem a [] = False
myelem a (x:xs) = x == a || myelem a xs

prop_myelem :: (Eq a) => a -> [a] -> Bool
prop_myelem a xs = myelem a xs == elem a xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (xs:xss) = xs ++ myconcat xss

prop_myconcat :: (Eq a) => [[a]] -> Bool
prop_myconcat xs = myconcat xs == concat xs

mymaximum :: [Int] -> Int
mymaximum [] = error "empty list"
mymaximum [x] = x
mymaximum (x:y:xs)
    | x > y = mymaximum (x:xs)
    | otherwise = mymaximum (y:xs)

prop_mymaximum :: [Int] -> Property
prop_mymaximum xs = (xs /= []) ==> mymaximum xs == maximum xs

myzip :: [a] -> [a] -> [(a,a)]
myzip [] ys = []
myzip xs [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

prop_myzip :: Eq a => [a] -> [a] -> Bool
prop_myzip xs ys = myzip xs ys == zip xs ys

--1-FP.9
r :: Num a => a -> a -> [a]
r a d = [a] ++ r (a+d) d

r1 :: Num a => Int -> a -> a -> a
r1 n a d = (r a d)!!(n - 1)

totalr :: Num a => Int -> Int -> a -> a -> a
totalr i j a d = sum (drop i (take j (r a d)))

--1-FP.10
allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)

diffList :: [Int] -> [Int]
difflist [] = error "Need list with length 2 or more"
difflist [x] = error "Need list with length 2 or more"
diffList [x,y] = [y - x]
diffList (x:y:xs) = (y - x) : diffList (y:xs)

isAS :: [Int] -> Bool
isAS xs
    | length xs >= 2 = allEqual (diffList xs)
    | otherwise = True

--1-FP.11
allRowsEquallyLong :: [[a]] -> Bool
allRowsEquallyLong [] = True
allRowsEquallyLong [xs] = True
allRowsEquallyLong (xs:ys:xss) = length xs == length ys && allRowsEquallyLong (ys:xss)

rowTotals :: Num a => [[a]] -> [a]
rowTotals [] = []
rowTotals (xs:xss) = sum xs : rowTotals xss

mytranspose :: [[a]] -> [[a]]
mytranspose ([]:x) = []
mytranspose xss = (map head xss) : mytranspose (map tail xss)

colTotals :: Num a => [[a]] -> [a]
colTotals xss = rowTotals (mytranspose xss)

--1-FP.12
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f xs = [x | x <- xs, f x]

prop_myfilter :: (Eq a) => (Fun a Bool) -> [a] -> Bool
prop_myfilter (Fun _ f) xs = myfilter f xs == filter f xs

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f a [] = a
myfoldl f a (x:xs) = myfoldl f (f a x) xs

myfoldr :: (a -> b -> a) -> a -> [b] -> a
myfoldr f a [] = a
myfoldr f a (xs) = myfoldr f (f a $ last xs) $ init xs

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] ys = []
myzipWith f xs [] = []
myzipWith f (x:xs) (y:ys) = f x y : myzipWith f xs ys

--1-FP.13
type Person = (Name, Age, Sex, Residence)
type Name = String
type Age = Int
data Sex = Male
    | Female
    deriving (Show, Eq, Ord)
type Residence = String

maarten :: Person
maarten = ("Maarten",19, Male, "Calslaan")
pepijn :: Person
pepijn = ("Pepijn",19, Female, "Hengeloooo")
piet :: Person
piet = ("Piet",25, Male, "Amsterdam")
henk :: Person
henk = ("Henk",69, Male, "Eindhoven")
petra :: Person
petra = ("Petra", 35, Female, "Loosdrecht")


getName :: Person -> Name
getName (n,a,s,r) = n

getAge :: Person -> Age
getAge (n,a,s,r) = a

getSex :: Person -> Sex
getSex (n,a,s,r) = s

getResidence :: Person -> Residence
getResidence (n,a,s,r) = r

increaseAgeRecursive :: [Person] -> Int -> [Person]
increaseAgeRecursive [] n = []
increaseAgeRecursive ((a,b,c,d):ps) n = (a,b+n,c,d) : increaseAgeRecursive ps n

increaseAgeListComp :: [Person] -> Int -> [Person]
increaseAgeListComp ps n = [(a,b+n,c,d) | (a,b,c,d) <- ps]

increaseAgeHighOrd :: [Person] -> Int -> [Person]
increaseAgeHighOrd ps n = map (\(p,q,r,s) -> (p,q+n,r,s)) ps

getWomenRecursive :: [Person] -> [Person]
getWomenRecursive [] = []
getWomenRecursive ((a,b,c,d):ps)
    | b > 30  && b < 40 && c == Female = (a,b,c,d) : getWomenRecursive ps
    | otherwise = getWomenRecursive ps

getWomenListComp :: [Person] -> [Person]
getWomenListComp ps = [(a,b,c,d) | (a,b,c,d) <- ps, b >= 30 && b <= 40 && c == Female]

getWomenHighOrd :: [Person] -> [Person]
getWomenHighOrd ps = filter (\(p,q,r,s) -> q >= 30 && q <= 40 && r == Female) ps

getAgeByName :: [Person] -> String -> Int
getAgeByName ps name = head [(b) | (a,b,c,d) <- ps, map toLower a == map toLower name]

sortByAge :: [Person] -> [Person]
sortByAge ps = [b | (a,b) <- sort $ zip (getAgeList ps) ps]
    where
        getAgeList :: [Person] -> [Int]
        getAgeList ps = [a | x <- ps, let a = getAge x]

--1-FP.14
sieve :: [Int]
sieve = removeMultiples [2..]
    where
        removeMultiples :: [Int] -> [Int]
        removeMultiples (x:xs) = x : removeMultiples [p | p <- xs, p `mod` x /= 0]

testPrime :: Int -> Bool
testPrime n = collapse sieve
    where
        collapse :: [Int] -> Bool
        collapse (x:xs) = x <= n && (x == n || collapse xs)

giveFirstPrimes :: Int -> [Int]
giveFirstPrimes n = take n sieve

giveSmallerPrimes :: Int -> [Int]
giveSmallerPrimes n = collapse n sieve
    where 
        collapse n (x:xs)
            | n < x = []
            | otherwise = x : collapse n xs

--1-FP.15




--QuickCheck
return []
check = $quickCheckAll