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

dig :: Parser Char
dig =  foldr (<|>) (char '0') (char <$> (['1'..'9']))

between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = (p1 *> p2) <* p3

--TODO: fix
whitespace :: Parser a -> Parser a
whitespace p1 = try (between (char ' ' <|> char '\n' <|> char '\t') p1 (char ' ' <|> char '\n' <|> char '\t')) <|> p1 

-- sep1 :: Parser a -> Parser b -> Parser [a]
-- sep1 p1 s = foldr (<|>) p1 (s *> )


-- sep :: Parser a -> Parser b -> Parser [a]

-- option :: a -> Parser a -> Parser a
-- option x

-- string :: String -> Parser String
-- string c = P p
--     where
--       p (Stream [])                    = []
--       p (Stream (x: xs )) | c == x     = [(x , Stream xs )]
--                           | otherwise  = []

-- identifier :: Parser String

-- integer :: Parser Integer

-- symbol :: String -> Parser String
-- symbol c = P p
--     where
--       p (Stream [])                    = []
--       p (Stream (x: xs )) | c == x     = [(x , Stream xs )]
--                           | otherwise  = []

-- parens :: Parser a -> Parser a

-- braces :: Parser a -> Parser a