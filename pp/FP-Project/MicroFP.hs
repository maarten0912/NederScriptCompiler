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

-- FP3.1
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

appEDSL = Prog [Func
        "functionName"
        [Identifier "arg1", Identifier "arg2"]
        (IfElse 
            (Cond (Integer 5) Smaller (Identifier "arg1"))
            (Integer 5)
            (Integer 7)
        )
        ]

-- FP3.2
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

appFibonacci = fibonacci

-- FP3.3
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

-- This is a typeclass so that you can use pretty on a whole EDSL and also
-- some partial EDSL's. Unfortunately you cannot use pretty on a Func or Cond
class Pretty a where
    pretty :: a -> IO()

instance Pretty Prog where
    --putStr is to make sure that \n is shown as new line
    pretty prog = putStr $ prettyP prog

instance Pretty Expr where
    pretty expr = putStr $ prettyE expr

instance Pretty Comp where
    pretty comp = putStr $ prettyC comp

appPretty = pretty fibonacci

-- FP3.4
eval :: Prog -> String -> [Integer] -> Integer
eval (Prog fs) fun args = calc (Prog fs) (bind as args e)
    where (Func funname as e) = head (filter (\(Func funname as e) -> funname == fun) fs)

-- This function is used to replace all the variables by their correct supplied value
bind :: [Expr] -> [Integer] -> Expr -> Expr
bind [] _ e = e
bind _ [] e = e
bind (fa:funargs) (v:values) expression = bind funargs values (replace expression fa v )

-- This function will replace a single variable by their given supplied value
replace :: Expr -> Expr -> Integer -> Expr
replace (Add x y) fa v = Add (replace x fa v) (replace y fa v)
replace (Sub x y) fa v = Sub (replace x fa v) (replace y fa v)
replace (Mult x y) fa v = Mult (replace x fa v) (replace y fa v)
replace (Parens x) fa v = replace x fa v
replace (IfElse (Cond a c b) x y) fa v = IfElse (Cond (replace a fa v) c (replace b fa v)) (replace x fa v) (replace y fa v)
replace (Call s []) fa v = Call s []
replace (Call s es) fa v = Call s (replaceArray es fa v)
replace e fa v | e == fa = (Integer v)
               | otherwise = e

-- This function will do the same as the previous function, except it will replace a variable in a whole array
replaceArray :: [Expr] -> Expr -> Integer -> [Expr]
replaceArray [] _ _ = []
replaceArray (e:es) fa v = (replace e fa v) : (replaceArray es fa v)

-- This function will actually calculate the result of an EDSL
calc :: Prog -> Expr -> Integer
calc p (Call s args)  = eval p s [a | arg <- args, let a = calc p arg]
calc p (Mult x y)     = calc p x * calc p y
calc p (Sub x y)      = calc p x - calc p y
calc p (Add x y)      = calc p x + calc p y
calc p (Parens x)     = calc p x
calc p (Integer x)    = x
calc p (Identifier x) = error ("Did not get enough arguments, no value for argument '" ++ x ++ "' found.")
calc p e              = calc p (reduce p e)

-- This function will reduce the IfElse statement to either the Then or the Else,
-- depending on the condition
reduce :: Prog -> Expr -> Expr
reduce p (IfElse c x y)   | calcCond p c == True = x
                          | otherwise = y
reduce p e                = e

-- This function will calculate if the IfElse should evaluate the Then or the Else
calcCond :: Prog -> Cond -> Bool
calcCond p (Cond e1 Smaller e2)     | calc p e1 < calc p e2 = True
                                    | otherwise = False
calcCond p (Cond e1 Equal e2)       | calc p e1 == calc p e2 = True
                                    | otherwise = False
calcCond p (Cond e1 Greater e2)     | calc p e1 > calc p e2 = True
                                    | otherwise = False

appEval = eval fib "fib" [10]

-- FP 4.1
prog :: Parser Prog
prog = whitespace f
    where f = Prog <$> (some func)

func :: Parser Func
func = whitespace f
    where f = Func <$> identifier <*> (many (Integer <$> integer <|> Identifier <$> identifier)) <* (symbol ":=") <*> expr <* (symbol ";")

expr :: Parser Expr
expr = whitespace f
    where f = Add <$> term <* (symbol "+") <*> expr
            <|> Sub <$> term <* (symbol "-") <*> expr
            <|> term

term :: Parser Expr
term = whitespace f
    where f = Mult <$> factor <* (symbol "*") <*> term <|> factor

factor :: Parser Expr
factor = whitespace f
    where f = Call <$> identifier <*> (parens (sep1 expr (char ',')))
            <|> IfElse <$> ((symbol "if") *> (parens (Cond <$> expr <*> comp <*> expr))) <* (symbol "then") <*> (braces expr) <* (symbol "else") <*> (braces expr)
            <|> Parens <$> parens expr
            <|> Identifier <$> identifier
            <|> Integer <$> integer

comp :: Parser Comp
comp = whitespace f
    where f = (\x -> Smaller) <$> (char '<') <|> (\x -> Equal) <$> (string "==") <|> (\x -> Greater) <$> (char '>')

appParserProg = runParser prog (Stream "fib n := if (n < 3) then { 1 } else { fib (n-1) + fib (n-2) };")

-- FP4.2
compile :: String -> Prog
compile x = fst . head $ runParser prog (Stream x)

appCompile = compile "fib n := if (n < 3) then { 1 } else { fib (n-1) + fib (n-2) };"

-- FP4.3
runFile :: FilePath -> [Integer] -> IO Integer
runFile path args =  (\x -> eval x (getLastFunctionName x) args) <$> (compile <$> readFile path)

-- This function will retrieve the name of the last function in a given EDSL
getLastFunctionName :: Prog -> String
getLastFunctionName (Prog fs) = funname
    where (Func funname as e) = last fs

-- We give an empty list of arguments because the last function of "functions.txt" has no arguments
appRunFile = runFile "functions.txt" []

return []
check = $quickCheckAll
-- QuickCheck: all prop_* tests