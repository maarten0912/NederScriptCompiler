-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Maarten Meijer (s2367114)
-- Student 2: Pepijn Visser (s2389428)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All


data Prog
    = Function Function

data Function
    = 


data Expr 
    = Term Term
    | Add Term Expr

data Term
    = Factor Factor
    | Mult Factor Term

data Factor
    = Const Integer
    | Identifier Expr Expr
    | IfElse Expr Ordering Expr Expr Expr

data Ordering = Smaller | Equal | Greater


-- 〈program〉  ::= (〈function〉)+
-- 〈function〉 ::= identifier (identifier | integer)∗ ’:=’〈expr〉’;’
-- 〈expr〉     ::=〈term〉
--                |〈term〉(’+’|’-’)〈expr〉
-- 〈term〉     ::=〈factor〉
--                |〈factor〉’*’〈term〉
-- 〈factor〉   ::= integer
--                | identifier(’(’〈expr〉(’,’〈expr〉)∗’)’)?
--                | ’if’ ’(’〈expr〉〈ordering〉〈expr〉’)’ ’then’ ’{’〈expr〉’}’ ’else’ ’{’〈expr〉’}’
--                | ’(’〈expr〉’)’
-- 〈ordering〉 ::= ’<’ | ’==’ | ’>’

-- data Expr = Const Integer
--           | Var String
--           | Mult Expr Expr
--           | Add Expr Expr
--           | If Cond Expr Expr
--           | Dec Expr
--           | Call String Expr
--           deriving Show

-- data Cond = Cond Expr Expr
--           deriving Show

-- data FunDef = FunDef String String Expr
--             deriving Show


-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
