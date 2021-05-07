module Set5 where

import Data.List
import Data.Functor
import Data.Either
import Data.Char

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Test.QuickCheck

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x -- Newer GHC versions contain a fromLeft :: l -> Either l r -> l

fromRight' :: Either l r -> r
fromRight' (Right x) = x -- Newer GHC versions contain a fromRight :: r -> Either l r -> r

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
          | otherwise  = fromRight' res
  where res = parse p "" xs


--3-FP.2
-- <expr>   ::== <term> ('+' <term>)*
-- <term>   ::== <factor> ('*' <factor>)*
-- <factor> ::== num
--             | identifier
--             | '(' <expr> ')'


languageDef =
  emptyDef { Token.identStart = letter
           , Token.identLetter = alphaNum
           , Token.reservedOpNames = ["+","*"]
           }
lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
integer = Token.integer lexer 
parens = Token.parens lexer
symbol = Token.symbol lexer
reserved = Token.reserved lexer

--3-FP.3
-- Expression EDSLs

-- <expr>   ::== <term> ('+' <term>)*
-- <term>   ::== <factor> ('*' <factor>)*
-- <factor> ::== num
--             | identifier
--             | '(' <expr> ')'

parseFactor :: Parser Expr
parseFactor =  (Const <$> integer) <|> (Var <$> identifier) <|> (parens parseExpr)

parseTerm :: Parser Expr
parseTerm = parseFactor `chainl1` mult

mult :: Parser (Expr -> Expr -> Expr)
mult = (\_ -> Mult) <$> (symbol "*")

parseExpr :: Parser Expr
parseExpr = parseDec <|> parseIf <|> try (parseTerm `chainl1` add)

add :: Parser (Expr -> Expr -> Expr)
add = (\_ -> Add) <$> (symbol "+")

--3-FP.4
-- <expr>      ::== <dec> | <term> ('+' <term>)* | <if>
-- <term>      ::== <factor> ('*' <factor>)*
-- <factor>    ::== num
--                | identifier
--                | '(' <expr> ')'
-- <condition> ::== <expr> '==' <expr>
-- <if>        ::== 'if' <condition> 'then' <expr> 'else' <expr>
-- <dec>       ::== 'dec' <expr> 

parseDec :: Parser Expr
parseDec = Dec <$> (symbol "dec" *> parseExpr)

parseCondition :: Parser Cond
parseCondition = Cond <$> (parseExpr <* symbol "==") <*> parseExpr

parseIf :: Parser Expr
parseIf = If <$> (symbol "if" *> parseCondition) <*> (symbol "then" *> parseExpr) <*> (symbol "else" *> parseExpr)

--3-FP.5
data Expr = Const Integer
          | Var String
          | Mult Expr Expr
          | Add Expr Expr
          | Func FunDef Expr
          | If Cond Expr Expr
          | Dec Expr
          deriving Show

data Cond = Cond Expr Expr
          deriving Show

data FunDef = FunDef String String Expr
            deriving Show

-- <function>  ::== 'function' identifier identifier '=' <expr>
-- <expr>      ::== <dec> | <term> ('+' <term>)* | <if>
-- <term>      ::== <factor> ('*' <factor>)*
-- <factor>    ::== num
--                | identifier
--                | '(' <expr> ')'
--                | identifier '(' <expr> ')'
-- <condition> ::== <expr> '==' <expr>
-- <if>        ::== 'if' <condition> 'then' <expr> 'else' <expr>
-- <dec>       ::== 'dec' <expr> 


parseFunc :: Parser Expr
parseFunc = Func <$> (symbol "function" *> identifier) <*> identifier <*> (symbol "=" *> parseExpr)

-- parserFun :: String -> FunDef
-- parserFun = parser parseFunc

-- fib :: FunDef
-- fib = parserFun
-- " function fib x = if x == 0 then 1 else ( if x == 1 then 1 else fib ( dec x )+ fib ( dec dec x )) "


-- fib :: Integer -> Integer
-- fib = (evalfun . parserFun) 
--  "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+fib(dec dec x))"

