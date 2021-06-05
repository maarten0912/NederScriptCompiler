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
    = Fun String FunArgs Expr

data FunArgs
    = NoArg
    | MoreIntArgs Integer FunArgs
    | MoreIdentArgs String FunArgs

data Expr
    = SingleExpr Term
    | Add Term Expr
    | Sub Term Expr

data Term
    = SingleTerm Factor
    | Mult Factor Expr

data Factor
    = Constant Integer
    | Identifier String CallArgs
    | IfElse Expr Orderings Expr Expr Expr
    | Parens Expr

data CallArgs
    = NoCallArg
    | OneArg Expr
    | MoreArgs Expr CallArgs

data Orderings = Smaller | Equal | Greater

-- 〈program〉  ::= (〈function〉)+
-- 〈function〉 ::= identifier (identifier | integer)∗ ’:=’〈expr〉’;’
-- 〈expr〉     ::=〈term〉|〈term〉(’+’|’-’)〈expr〉
-- 〈term〉     ::=〈factor〉|〈factor〉’*’〈term〉
-- 〈factor〉   ::= integer
--                | identifier ( ’(’〈expr〉( ’,’〈expr〉)∗ ’)’ )?
--                | ’if’ ’(’〈expr〉〈ordering〉〈expr〉’)’ ’then’ ’{’〈expr〉’}’ ’else’ ’{’〈expr〉’}’
--                | ’(’〈expr〉’)’
-- 〈ordering〉 ::= ’<’ | ’==’ | ’>’

-- fibonacci :: Prog
-- fibonacci = 
--     MoreFunctions (
--         Fun (
--             "fibonacci",
--             MoreIntArgs (0, NoArg),
--             SingleExpr (
--                 SingleTerm (
--                     Constant (0)
--                 )
--             )
--         )
--         MoreFunctions (
--             Fun (
--                 "fibonacci",
--                 MoreIntArgs (1, NoArg),
--                 SingleExpr (
--                     SingleTerm (
--                         Constant (1)
--                     )
--                 )
--             )
--             OneFunction (
--                 Fun (
--                     "fibonacci",
--                     MoreIdentArgs ("n", NoArg), 
--                     Add (
--                         SingleTerm (
--                             Identifier (
--                                 "fibonacci",
--                                 OneArg (
--                                     Sub (
--                                         SingleTerm (Identifier ("n", NoCallArg)),
--                                         SingleExpr (SingleTerm (Constant (1)))
--                                     ) 
--                                 )
--                             )
--                         )
--                         SingleExpr (
--                             SingleTerm (
--                                 Identifier (
--                                     "fibonacci",
--                                     OneArg (
--                                         Sub (
--                                             SingleTerm (Identifier ("n", NoCallArg)),
--                                             SingleExpr (SingleTerm (Constant (2)))
--                                         ) 
--                                     )
--                                 )
--                             )
--                         )
--                     )
--                 )
--             )
--         )
--     )




-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
