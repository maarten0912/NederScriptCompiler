{-# LANGUAGE TemplateHaskell #-}
module Exercises2 where
import FPPrac.Trees
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function
import Data.List
import Data.Maybe
import Data.Char

--data RoseTree = RoseNode String [RoseTree]

--2-FP.1
data Tree1a = Leaf1a Int
            | Node1a Int Tree1a Tree1a
            deriving (Show, Eq)

treea :: Tree1a
treea = Node1a 69 (Leaf1a 420)
                 (Node1a 42 (Leaf1a 666)
                            (Leaf1a 314))

pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a x) = RoseNode (show x) []
pp1a (Node1a x a b) = RoseNode (show x) [pp1a a,pp1a b]

data Tree1b = Leaf1b (Int, Int)
            | Node1b (Int, Int) Tree1b Tree1b
            deriving (Show, Eq)

treeb :: Tree1b
treeb = Node1b (0,69) (Leaf1b (1,420))
                 (Node1b (2,42) (Leaf1b (3,666))
                            (Leaf1b (4,314)))

pp1b :: Tree1b -> RoseTree
pp1b (Leaf1b x) = RoseNode (show x) []
pp1b (Node1b x a b) = RoseNode (show x) [pp1b a,pp1b b]

data Tree1c = Leaf1c Int
            | Node1c Tree1c Tree1c
            deriving (Show, Eq)

treec :: Tree1c
treec = Node1c (Leaf1c 420)
                 (Node1c (Leaf1c 666)
                            (Leaf1c 69))

pp1c :: Tree1c -> RoseTree
pp1c (Leaf1c x) = RoseNode (show x) []
pp1c (Node1c a b) = RoseNode "" [pp1c a,pp1c b]

data Tree1d = Leaf1d (Int,Int)
            | Node1d [Tree1d]
            deriving (Show, Eq)

treed :: Tree1d
treed = Node1d [
                Leaf1d (0,420),
                Node1d [
                        Leaf1d (69,666),
                        Leaf1d (1000,31415)
                        ],
                Leaf1d (1,6969)
                ]

pp1d :: Tree1d -> RoseTree
pp1d (Leaf1d x) = RoseNode (show x) []
pp1d (Node1d a) = RoseNode "" (map pp1d a)

class PP a where
    pp :: a -> RoseTree

instance PP Tree1a where
    pp t = pp1a t 

instance PP Tree1b where
    pp t = pp1b t 

instance PP Tree1c where
    pp t = pp1c t 

instance PP Tree1d where
    pp t = pp1d t 

--2-FP.2
mapTree :: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a x) = (Leaf1a (f x))
mapTree f (Node1a x p q) = (Node1a (f x) (mapTree f p) (mapTree f q))

treeAdd :: Tree1a -> Int -> Tree1a
treeAdd t a = mapTree (+a) t

treeSquare :: Tree1a -> Tree1a
treeSquare t = mapTree (\x -> x * x) t

addNode :: Tree1b -> Tree1a
addNode (Leaf1b (a,b)) = Leaf1a (a+b)
addNode (Node1b (a,b) p q) = Node1a (a+b) (addNode p) (addNode q)

mapTree1b :: ((Int,Int) -> Int) -> Tree1b -> Tree1a
mapTree1b f (Leaf1b x) = (Leaf1a (f x))
mapTree1b f (Node1b x p q) = (Node1a (f x) (mapTree1b f p) (mapTree1b f q))

--mapTree1b (\(x,y) -> x + y) treeb
--mapTree1b (\(x,y) -> x * y) treeb

--2-FP.3
binMirror1a :: Tree1a -> Tree1a
binMirror1a (Leaf1a x) = (Leaf1a x)
binMirror1a (Node1a x p q) = Node1a x (binMirror1a q) (binMirror1a p)
    
class BinMirror a where
    binMirror :: a -> a

instance BinMirror Tree1a where
    binMirror t = binMirror1a t

binMirror1d :: Tree1d -> Tree1d
binMirror1d (Leaf1d (a,b)) = Leaf1d (b,a)
binMirror1d (Node1d xs) = Node1d (map binMirror1d (reverse xs))
    
instance BinMirror Tree1d where
    binMirror t = binMirror1d t
    
--2-FP.4
data TreeInt = Leaf
            | Node Int TreeInt TreeInt
            deriving (Show, Eq)

sortedTree :: TreeInt
sortedTree = Node 5 (Node 3 (Node 1 (Leaf) (Leaf)) (Leaf)) (Node 6 (Leaf) (Node 8 (Leaf) (Leaf)))

pptreeint :: TreeInt -> RoseTree
pptreeint (Leaf) = RoseNode "" []
pptreeint (Node x a b) = RoseNode (show x) [pptreeint a,pptreeint b]

binM :: TreeInt -> TreeInt
binM (Leaf) = (Leaf)
binM (Node x p q) = Node x (binM q) (binM p)

instance PP TreeInt where
    pp t = pptreeint t 

instance BinMirror TreeInt where
    binMirror t = binM t

insertTree :: TreeInt -> Int -> TreeInt
insertTree (Leaf) a = Node a Leaf Leaf
insertTree (Node x p q) a
    | a < x = Node x (insertTree p a) q
    | otherwise = Node x p (insertTree q a)

makeTreeFold :: [Int] -> TreeInt
makeTreeFold xs = foldl (insertTree) Leaf xs

makeTreeRec :: [Int] -> TreeInt
makeTreeRec xs = helperFunc xs Leaf
    where 
        helperFunc :: [Int] -> TreeInt -> TreeInt
        helperFunc [] t = t
        helperFunc (x:xs) t = helperFunc xs (insertTree t x)

makeList :: TreeInt -> [Int]
makeList (Leaf) = []
makeList (Node x a b) = makeList a ++ [x] ++  makeList b

sortWithTree :: [Int] -> [Int]
sortWithTree xs = makeList $ makeTreeFold xs

prop_sort :: [Int] -> Bool
prop_sort xs = sort xs == sortWithTree xs

sortTree :: TreeInt -> TreeInt
sortTree t = (makeTreeFold . makeList) t

--2-FP.5
subtreeAt :: TreeInt -> Int -> Maybe TreeInt
subtreeAt (Leaf) a = Nothing
subtreeAt (Node x p q) a
    | x == a = Just (Node x p q)
    | a < x = subtreeAt p a
    | otherwise = subtreeAt q a

--2-FP.6
cutOffAt :: Tree1a -> Int -> Tree1a
cutOffAt (Leaf1a a) _ = Leaf1a a
cutOffAt (Node1a x a b) 0 = Leaf1a x
cutOffAt (Node1a x a b) i = Node1a x (cutOffAt a (i - 1)) (cutOffAt b (i - 1))

--2-FP.7
data BinTree a = LeafBin
               | NodeBin a (BinTree a) (BinTree a)
               deriving (Show, Eq)

instance (Show a) => PP (BinTree a) where
    pp LeafBin = RoseNode "" []
    pp (NodeBin x b c) = RoseNode (show x) [pp b,pp c]

instance BinMirror (BinTree a) where
    binMirror LeafBin = LeafBin
    binMirror (NodeBin a p q) = NodeBin a (binMirror q) (binMirror p)

instance Functor BinTree where
    fmap fun LeafBin = LeafBin
    fmap fun (NodeBin a p q) = NodeBin (fun a) (fmap fun p) (fmap fun q)

--2-FP.8
data MyList a = Nil 
              | Cons a ( MyList a)
              deriving (Show, Eq)

mylst = Cons 1 $ Cons 2 $ Cons 3 $ Nil

instance Functor MyList where
    fmap fun Nil = Nil
    fmap fun (Cons x xs) = Cons (fun x) (fmap fun xs) 

fromList :: [a] -> MyList a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

prop_functor :: (Eq a) => [a] -> Bool
prop_functor xs = let l = fromList xs in fmap id l == l

--2-FP.9
data Person = Person { 
                       name      :: String,
                       age       :: Int,
                       sex       :: Sex,
                       residence :: String 
                     } deriving     Show

data Sex = Male
         | Female
         deriving (Show, Eq)

maarten :: Person
maarten = Person "Maarten" 19 Male "Calslaan" 
pepijn :: Person
pepijn = Person "Pepijn" 19 Male "Hengeloooo"
mila :: Person
mila = Person "Mila" 19 Female "Calslaan"

plus :: Int -> [Person] -> [Person]
plus n ps = map (\p -> p { age = (age p) + n}) ps

allNames :: [Person] -> [String]
allNames ps = map (\p -> name p) ps

--2-FP.10
getInt :: IO Integer
getInt = fmap read getLine

--QuickCheck
return []
check = $quickCheckAll