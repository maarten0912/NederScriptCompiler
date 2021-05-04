--2-FP.11

data BinTree a b = LeafBin
               | NodeBin a b (BinTree a b) (BinTree a b)
               deriving (Show, Eq)

--2-FP.12

-- <expr> ::= <term> `+` <expr>
--        |  <term>
-- <term> ::= <factor> `*` <term>
--        |  <factor>
-- <factor> ::= num

parseFactor

-- parseTerm

-- parseExpr