module Set6 where

import Data.Foldable
import Data.Monoid
import Text.Read
import Data.Char
import Control.Applicative
import Test.QuickCheck

--import Set5

------------------------------------------------------------------------------
-- Exercise 3-FP.7
------------------------------------------------------------------------------

addstr :: String -> String -> Maybe String      
addstr xs ys = (show) <$> ((+) <$> (readMaybe xs) <*> (readMaybe ys))

------------------------------------------------------------------------------
-- Exercise 3-FP.8
------------------------------------------------------------------------------

data MyList a = Nil | Cons a (MyList a)
                    deriving (Show, Eq)
                    
mylst = Cons 1 $ Cons 2 $ Cons 3 $ Nil
mylst2 = Cons 10 $ Cons 20 $ Cons 30 $ Nil
mylst3 = Cons 100 $ Cons 200 $ Cons 300 $ Nil
mylst4 = Cons 100 $ Cons 200 $ Nil


myzipWith3' :: (a->b->c->d) -> MyList a->MyList b->MyList c->MyList d
myzipWith3' _ Nil _ _ = Nil
myzipWith3' _ _ Nil _ = Nil
myzipWith3' _ _ _ Nil = Nil
myzipWith3' f (Cons a as) (Cons b bs) (Cons c cs) = Cons (f a b c) (myzipWith3' f as bs cs)

instance Functor MyList where
    fmap fun Nil = Nil
    fmap fun (Cons x xs) = Cons (fun x) (fmap fun xs) 

instance Applicative MyList where
      pure x = Cons x Nil
      Nil <*> _ = Nil
      _ <*> Nil = Nil
      (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)
      
myzipWith :: (a->b->c) ->MyList a->MyList b->MyList c
myzipWith _ Nil _ = Nil
myzipWith _ _ Nil = Nil
myzipWith f as bs = f <$> as <*> bs

myzipWith3 :: (a->b->c->d) -> MyList a->MyList b->MyList c->MyList d
myzipWith3 _ Nil _ _ = Nil
myzipWith3 _ _ Nil _ = Nil
myzipWith3 _ _ _ Nil = Nil
myzipWith3 f as bs cs = f <$> as <*> bs <*> cs

------------------------------------------------------------------------------
-- Exercise 3-FP.9
------------------------------------------------------------------------------

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)

t :: BinTree Int
t = Node (Node
            (Leaf 3)
            (Node (Leaf 6)
                  (Leaf 9)))
         (Leaf 9)
    
t2 :: BinTree Int
t2 = Node (Leaf 3) (Leaf 5)

inftree :: BinTree Int
inftree = Node (Leaf 1) inftree

------------------------------------------------------------------------------
-- Exercise 3-FP.11
------------------------------------------------------------------------------


data A a = A { fromA :: Maybe a }
         deriving Show

------------------------------------------------------------------------------
-- Exercise 3-FP.13
------------------------------------------------------------------------------

--fact = parserFun "function factorial x = if x == 0 then 1 else factorial(dec x) * x"
