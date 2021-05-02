grammar LRQ;

//@header{package pp.block2.cc.antlr;}

l : r A
  | q B A
  ;

r : A B A rprime
  | C A B A rprime
  ;

rprime : B C rprime
  |
  ;

q : B qprime
  ;

qprime : B C
  | C
  ;


A : 'a';
B : 'b';
C : 'c';

// ignore whitespace
WS : [ \t\n\r] -> skip;

// everything else is a typo
TYPO : [a-zA-Z]+;
