grammar LRQ;

//@header{package pp.block2.cc.antlr;}

l : r A
  | q B A
  ;

r : A B A
  | C A B A
  | r B C
  ;

q : B B C
  | B C
  ;

A : 'a';
B : 'b';
C : 'c';

// ignore whitespace
WS : [ \t\n\r] -> skip;

// everything else is a typo
TYPO : [a-zA-Z]+;
