grammar SimpleArithmetic;

import SimpleArithmeticVocab;

t    : t HAT t # hat
     | t PLUS t  # plus
     | t EQUALS t  # equals
     | LPAR t RPAR # bracket
     | NUMBER # number
     | BOOL # bool
     | STR # string
     ;
