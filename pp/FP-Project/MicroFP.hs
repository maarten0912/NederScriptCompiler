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
    deriving (Show, Eq)

data Func = Func String [Expr] Expr
    deriving (Show, Eq)

data Expr = Integer Integer
          | Identifier String
          | Call String [Expr]
          | IfElse Cond Expr Expr
          | Parens Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          deriving (Show, Eq)

data Cond = Cond Expr Comp Expr
    deriving (Show, Eq)

data Comp = Smaller | Equal | Greater
    deriving (Show, Eq)

-- fib n := if (n < 3) then { 1 } else { fib (n-1) + fib (n-2)};

prog :: Parser Prog
prog = Prog <$> (some func)

func :: Parser Func
func = Func <$> identifier <*> (many (Integer <$> integer <|> Identifier <$> identifier)) <* (symbol ":=") <*> expr <* (symbol ";")

expr :: Parser Expr
expr = Add <$> term <* (symbol "+") <*> expr
    <|> Sub <$> term <* (symbol "-") <*> expr
    <|> term

term :: Parser Expr
term = Mult <$> factor <* (symbol "*") <*> term <|> factor

factor :: Parser Expr
factor = Call <$> identifier <*> (parens (sep1 expr (char ',')))
    <|> IfElse <$> ((symbol "if") *> (parens (Cond <$> expr <*> comp <*> expr))) <* (symbol "then") <*> (braces expr) <* (symbol "else") <*> (braces expr)
    <|> Parens <$> parens expr
    <|> Identifier <$> identifier
    <|> Integer <$> integer

comp :: Parser Comp
comp = (\x -> Smaller) <$> (char '<') <|> (\x -> Equal) <$> (string "==") <|> (\x -> Greater) <$> (char '>')

compile :: String -> Prog
compile x = fst . head $ runParser prog (Stream x)

eval :: Prog -> String -> [Integer] -> Integer
eval (Prog []) fun args = error ("No function with the name \"" ++ fun ++ "\" found")
eval (Prog ((Func funname as e):fs)) fun args | funname == fun = calc (bind as args e)
                                              | otherwise = eval (Prog fs) fun args

--     funargs    values    expression  bound expression
bind :: [Expr] -> [Integer] -> Expr -> Expr
bind [] [] e = e
bind (fa:funargs) (v:values) expression = bind funargs values (replace expression fa v )

replace :: Expr -> Expr -> Integer -> Expr
replace (Add x y) fa v = Add (replace x fa v) (replace y fa v)
replace (Sub x y) fa v = Sub (replace x fa v) (replace y fa v)
replace (Mult x y) fa v = Mult (replace x fa v) (replace y fa v)
replace (Parens x) fa v = replace x fa v
replace (IfElse c x y) fa v = IfElse c (replace x fa v) (replace y fa v)
replace (Call s []) fa v = Call s []
replace (Call s es) fa v = Call s (replaceArray es fa v)
replace e fa v | e == fa = (Integer v)
               | otherwise = e

replaceArray :: [Expr] -> Expr -> Integer -> [Expr]
replaceArray [] _ _ = []
replaceArray (e:es) fa v = (replace e fa v) : (replaceArray es fa v)

calc :: Expr -> Integer
calc (Mult x y)     = calc x * calc y
calc (Sub x y)      = calc x - calc y
calc (Add x y)      = calc x + calc y
calc (Parens x)     = calc x
calc (Integer x)    = x
calc e              = calc (reduce e)

reduce :: Expr -> Expr
reduce (IfElse c x y)   | calcCond c == True = x
                        | otherwise = y
reduce e                = e

calcCond :: Cond -> Bool
calcCond (Cond e1 Smaller e2)   | calc e1 < calc e2 = True
                                | otherwise = False
calcCond (Cond e1 Equal e2)     | calc e1 == calc e2 = True
                                | otherwise = False
calcCond (Cond e1 Greater e2)   | calc e1 > calc e2 = True
                                | otherwise = False

runFile :: FilePath ->  IO String
runFile path = readFile path

prettyP :: Prog -> String
prettyP (Prog []) = ""
prettyP (Prog ((Func s es e1):fs)) = s ++ (foldl (\x y-> x ++ " " ++ (prettyE y)) "" es) ++ " := " ++ prettyE e1 ++ ";\n" ++ prettyP (Prog fs)

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


fibonacci :: Prog
fibonacci =
    Prog [
        Func "fibonacci" [Integer 0] (Integer 0),
        Func "fibonacci" [Integer 1] (Integer 1),
        Func "fibonacci" [Identifier "n"] 
            (Add 
                (Call "fibonacci" [Sub (Identifier "n") (Integer 1)]) 
                (Call "fibonacci" [Sub (Identifier "n") (Integer 2)])
            )
    ]

fib :: Prog
fib =
    Prog [
        Func "fib" [Identifier "n"] 
            (IfElse (Cond (Identifier "n") Smaller (Integer 3))
                (Integer 1)
                (Add
                     (Call "fib" [Sub (Identifier "n") (Integer 1)])
                     (Call "fib" [Sub (Identifier "n") (Integer 2)])
                )
            )
    ]

sum :: Prog
sum = 
    Prog [
        Func "sum" [Integer 0] (Integer 0),
        Func "sum" [Identifier "a"] (Add (Call "sum" [Sub (Identifier "a") (Integer 1)])(Identifier "a"))
    ]

div :: Prog
div =
    Prog [
        Func "div" [Identifier "x", Identifier "y"] 
            (IfElse (Cond (Identifier "x") Smaller (Identifier "y")) 
                (Integer 0)
                (Add (Integer 1) (Call "div" [Sub (Identifier "x")(Identifier "y"), Identifier "y"]))
            )
    ]

twice :: Prog
twice =
    Prog [
        Func "twice" [Identifier "f", Identifier "x"] (Call "f" [Call "f" [Identifier "x"]]) 
    ]

add :: Prog
add =
    Prog [
        Func "add" [Identifier "x", Identifier "y"] (Add (Identifier "x") (Identifier "y"))
    ]

inc :: Prog
inc = 
    Prog [
        Func "inc" [] (Call "add" [Integer 1])
    ]

eleven :: Prog
eleven =
    Prog [
        Func "eleven" [] (Call "inc" [Integer 10])
    ]



return []
check = $quickCheckAll
-- QuickCheck: all prop_* tests