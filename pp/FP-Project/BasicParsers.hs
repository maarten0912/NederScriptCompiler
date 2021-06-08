-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Maarten Meijer (s2367114)
-- Student 2: Pepijn Visser (s2389428)

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck
import PComb


--FP2.1
letter :: Parser Char
letter =  foldr (<|>) (char 'a') (char <$> (['b'..'z'] ++ ['A' .. 'Z']))

dig :: Parser Char
dig =  foldr (<|>) (char '0') (char <$> ['1'..'9'])

-- Example for letter and dig
letterEx = runParser letter $ Stream "a"
digEx = runParser dig $ Stream "1"

--FP2.2
between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = (p1 *> p2) <* p3

whitespace :: Parser a -> Parser a
whitespace p1 = between (many (char ' ' <|> char '\n' <|> char '\t')) p1 (many (char ' ' <|> char '\n' <|> char '\t'))

-- Example for between and whitespace
betweenEx = runParser (between dig letter dig) $ Stream "1a1"
whiteEx = runParser (whitespace letter) $ Stream "    a  a  "

--FP2.3
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p1 s = (:) <$> p1 <*> many (s *> p1)

sep :: Parser a -> Parser b -> Parser [a]
sep p1 s = sep1 p1 s <|> pure []

option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- Example for sep1, sep and option
sep1Ex = runParser (sep1 letter (char ',')) $ Stream "a,b,c,d"
sepEx = runParser (sep letter (char ',')) $ Stream ","
optionEx = runParser (option 'a' letter) $ Stream "123"

--FP2.4
string :: String -> Parser String
string [] = pure ""
string (x:xs) = (:) <$> char x <*> string xs

identifier :: Parser String
identifier = whitespace ((:) <$> lowerLetter <*> many (lowerLetter <|> dig))

lowerLetter :: Parser Char
lowerLetter =  foldr (<|>) (char 'a') (char <$> ['b'..'z'])

integer :: Parser Integer
integer = whitespace (read <$> (some dig))

symbol :: String -> Parser String
symbol xs = whitespace (string xs)

parens :: Parser a -> Parser a
parens p = between (char '(') p (char ')')

braces :: Parser a -> Parser a
braces p = between (char '{') p (char '}')

-- Examples for the functions above
stringEx = runParser (string "hello") $ Stream "hello world"
identifierEx = runParser (identifier) $ Stream "  a  "
integerEx = runParser (integer) $ Stream "  1  "
symbolEx = runParser (symbol "hello") $ Stream "  hello world  "
parensEx = runParser (parens (string "hello there")) $ Stream "(hello there)"
bracesEx = runParser (braces (string "hello there")) $ Stream "{hello there}"
