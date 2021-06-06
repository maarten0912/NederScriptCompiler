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

data Prog = Prog [Func]
    deriving Show

data Func = Func String Expr Expr
    deriving Show

data Expr = Integer Integer
          | Identifier String
          | Call String [Expr]
          | IfElse Cond Expr Expr
          | Parens Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          deriving Show

data Cond = Cond Expr Comp Expr
    deriving Show

data Comp = Smaller | Equal | Greater
    deriving Show

prog :: Parser Prog
prog = Prog <$> (some func)

func :: Parser Func
func = Func <$> identifier <*> (Identifier <$> identifier <|> Integer <$> integer) <* (symbol ":=") <*> expr <* (symbol ";")

expr :: Parser Expr
expr = Call <$> identifier <*> (parens (sep1 expr (char ',')))
    -- waarom tf kan dit niet: IfElse <$> (symbol "if") *> (parens bla bla bla
    <|> (\a b c d -> IfElse b c d) <$> (symbol "if") <*> (parens (Cond <$> expr <*> comp <*> expr)) <* (symbol "then") <*> (braces expr) <* (symbol "else") <*> (braces expr)
    <|> Parens <$> parens expr
    -- <|> Add <$> expr <* (symbol "+") <*> expr
    -- <|> Sub <$> expr <* (symbol "-") <*> expr
    -- <|> Mult <$> expr <* (symbol "*") <*> expr
    <|> Identifier <$> identifier
    <|> Integer <$> integer

comp :: Parser Comp
comp = (\x -> Smaller) <$> (char '<') <|> (\x -> Equal) <$> (string "==") <|> (\x -> Greater) <$> (char '>')

prettyP :: Prog -> String
prettyP (Prog []) = ""
prettyP (Prog ((Func s e1 e2):fs)) = s ++ " " ++ prettyE e1 ++ ":= " ++ prettyE e2 ++ ";\n" ++ prettyP (Prog fs)

prettyE :: Expr -> String
prettyE (Integer i) = show i
prettyE (Identifier i) = i
prettyE (Call s (e:es)) = s ++ " (" ++ (prettyE e) ++ (foldl (\x y-> x ++ ", " ++ (prettyE y)) "" es) ++ ")"
prettyE (IfElse (Cond e1 c e2) e3 e4) = "if (" ++ prettyE e1 ++ " " ++ prettyC c ++ " " ++ prettyE e2 ++ ") then { " ++ prettyE e3 ++ " } else { " ++ prettyE e4 ++ " }"
prettyE (Parens e) = "(" ++ prettyE e ++ ")"
prettyE (Add e1 e2) = prettyE e1 ++ " + " ++ prettyE e2
prettyE (Sub e1 e2) = prettyE e1 ++ " - " ++ prettyE e2
prettyE (Mult e1 e2) = prettyE e1 ++ " * " ++ prettyE e2

prettyC :: Comp -> String
prettyC Smaller = "<"
prettyC Equal = "=="
prettyC Greater = ">"

class Pretty a where
    pretty :: a -> IO()

instance Pretty Prog where
    --putStr is to make sure that \n is shown as new line
    pretty prog = putStr $ prettyP prog

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
