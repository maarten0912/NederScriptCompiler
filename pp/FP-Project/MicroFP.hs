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
    = OneFunction Function
    | MoreFunctions Function Prog

data Function
    = Fun String Args Expr

data Args
    = NoArg
    | MoreIntArgs Integer Args
    | MoreIdentArgs String Args

data Expr
    = SingleExpr Term
    | Add Term Expr
    | Sub Term Expr

data Term
    = SingleTerm Factor
    | Mult Factor Expr

data Factor
    = Const Integer
    | Identifier String CallArgs
    | IfElse Expr Ordering Expr Expr Expr
    | Parens Expr

data CallArgs
    = NoArg
    | OneArg Expr
    | MoreArgs Expr CallArgs

data Ordering = Smaller | Equal | Greater

-- 〈program〉  ::= (〈function〉)+
-- 〈function〉 ::= identifier (identifier | integer)∗ ’:=’〈expr〉’;’
-- 〈expr〉     ::=〈term〉|〈term〉(’+’|’-’)〈expr〉
-- 〈term〉     ::=〈factor〉|〈factor〉’*’〈term〉
-- 〈factor〉   ::= integer
--                | identifier ( ’(’〈expr〉( ’,’〈expr〉)∗ ’)’ )?
--                | ’if’ ’(’〈expr〉〈ordering〉〈expr〉’)’ ’then’ ’{’〈expr〉’}’ ’else’ ’{’〈expr〉’}’
--                | ’(’〈expr〉’)’
-- 〈ordering〉 ::= ’<’ | ’==’ | ’>’



-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
