-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Maarten Meijer (s2367114)
-- Student 2: Pepijn Visser (s2389428)

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck
import PComb

letter :: Parser Char
letter =  foldr (<|>) (char 'a') (char <$> (['b'..'z'] ++ ['A' .. 'Z']))

lowerLetter :: Parser Char
lowerLetter =  foldr (<|>) (char 'a') (char <$> (['b'..'z']))


dig :: Parser Char
dig =  foldr (<|>) (char '0') (char <$> (['1'..'9']))

between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = (p1 *> p2) <* p3

--TODO: fix
whitespace :: Parser a -> Parser a
whitespace p1 = between (many (char ' ' <|> char '\n' <|> char '\t')) p1 (many (char ' ' <|> char '\n' <|> char '\t'))

sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p1 s = (:) <$> p1 <*> (many ((\x y -> y) <$> s <*> p1))


-- p1 <|> 
-- p1 <*> s <*> p1 <|>
-- p1 <*> s <*> p1 <*> s <*> p1 <|>
-- p1 <*> s <*> p1 <*> s <*> p1 <*> s <*> p1 <|>
-- ...

sep :: Parser a -> Parser b -> Parser [a]
sep p1 s = sep1 p1 s <|> pure []

option :: a -> Parser a -> Parser a
option x p = p <|> pure x


string :: String -> Parser String
string [] = pure ""
string (x:xs) = (:) <$> (char x) <*> (string xs)

identifier :: Parser String
identifier = (:) <$> lowerLetter <*> (many (lowerLetter <|> dig))

integer :: Parser Integer
integer = read <$> whitespace (many dig)

-- symbol :: String -> Parser String
-- symbol xs = whitespace (string xs)

parens :: Parser a -> Parser a
parens p = between (char '(') p (char ')')

braces :: Parser a -> Parser a
braces p = between (char '{') p (char '}')