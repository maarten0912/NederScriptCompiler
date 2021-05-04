{-# LANGUAGE TemplateHaskell #-}
module Exercises2part4 where
import FPPrac.Trees
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function
import Data.List
import Data.Maybe
import Data.Char

--2-FP.11

data BinTree a b = LeafBin
               | NodeBin a b (BinTree a b) (BinTree a b)
               deriving (Show, Eq)

--2-FP.12

-- <expr> ::= <term> `+` <expr>
--        |  <term>
-- <term> ::= <factor> `*` <term>
--        |  <factor>
-- <factor> ::= num

parseFactor

-- parseTerm

-- parseExpr