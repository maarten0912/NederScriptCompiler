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
    = OneFunction String FunArgs Expr
    | MoreFunctions String FunArgs Expr Prog
    deriving Show

data FunArgs
    = NoArg
    | MoreIntArgs Integer FunArgs
    | MoreIdentArgs String FunArgs
    deriving Show

data Expr
    = SingleExpr Term
    | Add Term Expr
    | Sub Term Expr
    deriving Show

data Term
    = SingleTerm Factor
    | Mult Factor Expr
    deriving Show

data Factor
    = Constant Integer
    | Identifier String CallArgs
    | IfElse Expr Orderings Expr Expr Expr
    | Parens Expr
    deriving Show

data CallArgs
    = NoCallArg
    | OneArg Expr
    | MoreArgs Expr CallArgs
    deriving Show

data Orderings = Smaller | Equal | Greater
                deriving Show

-- 〈program〉  ::= (〈function〉)+
-- 〈function〉 ::= identifier (identifier | integer)∗ ’:=’〈expr〉’;’
-- 〈expr〉     ::=〈term〉|〈term〉(’+’|’-’)〈expr〉
-- 〈term〉     ::=〈factor〉|〈factor〉’*’〈term〉
-- 〈factor〉   ::= integer
--                | identifier ( ’(’〈expr〉( ’,’〈expr〉)∗ ’)’ )?
--                | ’if’ ’(’〈expr〉〈ordering〉〈expr〉’)’ ’then’ ’{’〈expr〉’}’ ’else’ ’{’〈expr〉’}’
--                | ’(’〈expr〉’)’
-- 〈ordering〉 ::= ’<’ | ’==’ | ’>’

pretty :: Prog -> String
pretty (OneFunction s fa e) = s ++ " " ++ (prettyFA fa) ++ ":= " ++ (prettyE e) ++ ";\n"
pretty (MoreFunctions s fa e p) = s ++ " " ++ (prettyFA fa) ++ " := " ++ (prettyE e) ++ ";\n" ++ (pretty p)

prettyFA :: FunArgs -> String
prettyFA NoArg = ""
prettyFA (MoreIntArgs i NoArg) = (show i)
prettyFA (MoreIntArgs i fa) = (show i) ++ " " ++ (prettyFA fa)
prettyFA (MoreIdentArgs s NoArg) = s ++ " "
prettyFA (MoreIdentArgs s fa) = s ++ " " ++ (prettyFA fa)

prettyE :: Expr -> String
prettyE (SingleExpr t) = prettyT t
prettyE (Add t e) = (prettyT t) ++ " + " ++ (prettyE e)
prettyE (Sub t e) = (prettyT t) ++ " - " ++ (prettyE e)

prettyT :: Term -> String
prettyT (SingleTerm f) = prettyF f
prettyT (Mult f e) = (prettyF f) ++ " * " ++ (prettyE e)

prettyF :: Factor -> String
prettyF (Constant c) = show c
prettyF (Identifier s NoCallArg) = s
prettyF (Identifier s ca) = s ++ " (" ++ (prettyCA ca) ++")"
prettyF (IfElse e1 o e2 e3 e4)
    = "if (" ++ (prettyE e1) ++ " " ++ (prettyO o) ++ " " ++ (prettyE e2)
        ++ ") then { " ++ (prettyE e3) ++ " } else { " ++ (prettyE e4) ++ " }"
prettyF (Parens e) = "(" ++ (prettyE e) ++ ")"

prettyCA :: CallArgs -> String
prettyCA (OneArg e) = prettyE e
prettyCA (MoreArgs e ca) = prettyE e ++ " " ++ (prettyCA ca)


prettyO :: Orderings -> String
prettyO Smaller = "<"
prettyO Equal = "=="
prettyO Greater = ">"

fibonacci :: Prog
fibonacci =
    MoreFunctions
        "fibonacci"
        (MoreIntArgs 0 NoArg)
        (SingleExpr (SingleTerm (Constant 0)))
        (MoreFunctions
                "fibonacci"
                (MoreIntArgs 1 NoArg)
                (SingleExpr (SingleTerm (Constant 1)))
                (OneFunction
                    "fibonacci"
                    (MoreIdentArgs "n" NoArg)
                    (Add
                        (SingleTerm
                            (Identifier
                                "fibonacci"
                                ( OneArg
                                    ( Sub
                                        (SingleTerm (Identifier "n" NoCallArg))
                                        (SingleExpr (SingleTerm (Constant 1)))
                                    )
                                )
                            )
                        )
                        (SingleExpr
                            (SingleTerm
                                (Identifier
                                    "fibonacci"
                                    (OneArg
                                        (Sub
                                            (SingleTerm (Identifier "n" NoCallArg))
                                            (SingleExpr (SingleTerm (Constant 2)))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )

fib :: Prog
fib =
    OneFunction
        "fib"
        (MoreIdentArgs "n" NoArg)
        (SingleExpr
            (SingleTerm
                (IfElse
                    (SingleExpr (SingleTerm (Identifier "n" NoCallArg)))
                    Smaller
                    (SingleExpr (SingleTerm (Constant 3)))
                    (SingleExpr (SingleTerm (Constant 1)))
                    (Add
                        (SingleTerm 
                            (Identifier
                                "fib"
                                (OneArg 
                                    (Sub
                                        (SingleTerm (Identifier "n" NoCallArg))
                                        (SingleExpr (SingleTerm (Constant 1)))
                                    )
                                )
                            )
                        )
                        (SingleExpr
                            (SingleTerm 
                                (Identifier
                                    "fib"
                                    (OneArg 
                                        (Sub
                                            (SingleTerm (Identifier "n" NoCallArg))
                                            (SingleExpr (SingleTerm (Constant 2)))
                                        )
                                    )
                                )
                            )
                        )
                        
                    )
                )
            )
        )

sum :: Prog
sum = 
    MoreFunctions
        "sum"
        (MoreIntArgs 0 NoArg)
        (SingleExpr (SingleTerm (Constant 0)))
        (OneFunctionpretty (MoreFunctions s NoArg e p) = s ++ " := " ++ (prettyE e) ++ ";\n" ++ (pretty p)

            "sum"
            (MoreIdentArgs "a" NoArg)
            (Add
                (SingleTerm 
                    (Identifier 
                        "sum" 
                        (OneArg
                            (Sub
                                (SingleTerm (Identifier "a" NoCallArg))
                                (SingleExpr (SingleTerm (Constant 1)))
                            )
                        )
                    )
                )
                (SingleExpr (SingleTerm (Identifier "a" NoCallArg)))
            )
        )

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
