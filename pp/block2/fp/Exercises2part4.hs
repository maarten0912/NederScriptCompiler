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
data BinTree a b = LeafBin b
               | NodeBin a (BinTree a b) (BinTree a b)
               deriving (Show, Eq)

--2-FP.12

-- <expr> ::= <term> `+` <expr>
--        |  <term>
-- <term> ::= <factor> `*` <term>
--        |  <factor>
-- <factor> ::= num
--          | identifier
--          | '(' <expr> ')'

data Value = Const Int
           | Id2 String
             deriving Show

la :: [Char] -> [Char] -> Bool --look-ahead of one token
la xs l = length xs > 0 && (head xs) `elem` l

parseFactor :: String -> (BinTree Char Value , String)
parseFactor (x:xs) | la (x:xs) ['0'..'9'] = (LeafBin (Const (read [x])), xs)
                   | isLetter x = (LeafBin (Id2 [x]), xs)
                   | otherwise = (t1,as)
                   where ('(':ys) = (x:xs)
                         (t1,zs) = parseExpr ys
                         (')':as) = zs

parseTerm :: String -> (BinTree Char Value, String)
parseTerm (x:xs) | la ys "*" = (NodeBin '*' t1 t2, as) -- <factor> * <term>
                 | otherwise = parseFactor (x:xs)              -- <factor>
    where (t1, ys) = parseFactor (x:xs)
          ('*':zs) = ys
          (t2, as) = parseTerm zs

parseExpr :: String -> (BinTree Char Value, String)
parseExpr (x:xs) | la ys "+" = (NodeBin '+' t1 t2, as) -- <term> + <expr>
                 | otherwise = parseTerm (x:xs)             -- <term>
    where (t1, ys) = parseTerm (x:xs)
          ('+':zs) = ys
          (t2, as) = parseExpr zs

--2-FP.13
data Token = LBrack         -- '('
           | RBrack         --')'
           | Plus           --'+'
           | Times          --'*'
           | Number Int     --[1-9][0-9]*
           | Id String      --[a-zA-Z][a-zA-Z0-9]+
           deriving (Show, Eq)

letter=['a'..'z'] ++ ['A'..'Z']
digit=['0'..'9']
letdig=letter ++ digit

tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer (' ':xs) = tokenizer xs
tokenizer ('(':xs) = LBrack : tokenizer xs
tokenizer (')':xs) = RBrack : tokenizer xs
tokenizer ('+':xs) = Plus : tokenizer xs
tokenizer ('*':xs) = Times : tokenizer xs
tokenizer (x:xs) | x `elem` digit = Number (read num) : tokenizer rem1
                 | x `elem` letter = Id id : tokenizer rem2
    where(num, rem1) = span (`elem` digit) (x:xs)
         (id, rem2)  = span (`elem` letdig) (x:xs)

--2-FP.14             
parseExpr' :: [Token] -> BinTree Token Value
parseExpr' xs | s /= [] = error "Cannot parse"
              | otherwise = t
    where (t,s) = helperExpr xs


helperla :: [Token] -> [Token] -> Bool --look-ahead of one token
helperla xs l = length xs > 0 && (head xs) `elem` l

helperExpr :: [Token] -> (BinTree Token Value, [Token])
helperExpr (x:xs) | helperla ys [Plus] = (NodeBin Plus t1 t2, as) -- <term> + <expr>
                  | otherwise = helperTerm (x:xs)             -- <term>
    where (t1, ys) = helperTerm (x:xs)
          ((Plus):zs) = ys
          (t2, as) = helperExpr zs

helperTerm :: [Token] -> (BinTree Token Value, [Token])
helperTerm (x:xs) | helperla ys [Times] = (NodeBin Times t1 t2, as) -- <factor> * <term>
                  | otherwise = helperFactor (x:xs)
    where (t1, ys) = helperFactor (x:xs)
          (Times:zs) = ys
          (t2, as) = helperTerm zs

helperFactor :: [Token] -> (BinTree Token Value, [Token])
helperFactor ((Number a):xs) = (LeafBin (Const a), xs)
helperFactor ((Id a):xs) = (LeafBin (Id2 a), xs)
helperFactor xs = (t1,as)
    where (LBrack:ys) = xs
          (t1,zs) = helperExpr ys
          (RBrack:as) = zs