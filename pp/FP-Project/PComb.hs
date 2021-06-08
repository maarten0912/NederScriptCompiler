-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Maarten Meijer (s2367114)
-- Student 2: Pepijn Visser (s2389428)

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- FP1.1
data Stream = Stream [Char]
              deriving (Eq, Show)

appStream = (Stream "abc") 

data Parser a = P {
    runParser :: Stream -> [(a, Stream)]
}

-- FP1.2
instance Functor Parser where
    fmap f p = P (\x -> [(f r, s) | (r,s) <- runParser p x])

appFunctor = runParser (ord <$> char 'a') (Stream "a")

-- FP1.3
char :: Char -> Parser Char
char c = P p
      where
      p (Stream [])                    = []
      p (Stream (x: xs )) | c == x     = [(x , Stream xs )]
                          | otherwise  = []

appChar = runParser (char 'a') (Stream "a")

parserOne :: Stream -> [(Char, Stream)]
parserOne = runParser (char '1')

-- Example for char
charEx = runParser (char 'a') (Stream "a")

-- FP1.4
failure :: Parser a
failure = P p
    where
    p (Stream xs) = []

appFailure = runParser failure (Stream "useless")

parserFailure :: Stream -> [(Char, Stream)]
parserFailure = runParser failure

-- Example for failure
failEx = runParser failure (Stream "")

-- FP1.5
instance Applicative Parser where
    pure p = P (\x -> [(p, x)])
    p1 <*> p2 = P (\s -> [ (r1 r2, s2)
                            | (r1, s1) <- runParser p1 s,
                              (r2, s2) <- runParser p2 s1 ])

appApplicative = runParser ((,) <$> (char 'a') <*> (char 'b')) (Stream "ab")

-- FP1.6
instance Alternative Parser where
    empty = P (\x -> [])
    p1 <|> p2 = P func
        where
            func x = if (length (par1)) > 0 then par1 else par2
                where
                    par1 = runParser p1 x
                    par2 = runParser p2 x

appAlternative = runParser ((char 'a') <|> (char 'b')) (Stream "b")