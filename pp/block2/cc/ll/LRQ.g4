lexer grammar LRQ;

A : 'a';
B : 'b';
C : 'c';

// ignore whitespace
WS : [ \t\n\r] -> skip;

// everything else is a typo
TYPO : [a-zA-Z]+;