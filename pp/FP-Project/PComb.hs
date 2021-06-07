-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Maarten Meijer (s2367114)
-- Student 2: Pepijn Visser (s2389428)

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

data Parser a = P {
    runParser :: Stream -> [(a, Stream)]
}

char :: Char -> Parser Char
char c = P p
      where
      p (Stream [])                    = []
      p (Stream (x: xs )) | c == x     = [(x , Stream xs )]
                          | otherwise  = []

parserOne :: Stream -> [(Char, Stream)]
parserOne = runParser (char '1')

failure :: Parser a
failure = P p
    where
    p (Stream xs) = []

parserFailure :: Stream -> [(Char, Stream)]
parserFailure = runParser failure

instance Functor Parser where
    fmap f p = P (\x -> [(f r, s) | (r,s) <- runParser p x])

instance Applicative Parser where
    pure p = P (\x -> [(p, x)])
    p1 <*> p2 = P (\s -> [ (r1 r2, s2)
                            | (r1, s1) <- runParser p1 s,
                              (r2, s2) <- runParser p2 s1 ])

instance Alternative Parser where
    empty = P (\x -> [])
    p1 <|> p2 = P func
        where
            func x = if (length (par1)) > 0 then par1 else par2
                where
                    par1 = runParser p1 x
                    par2 = runParser p2 x