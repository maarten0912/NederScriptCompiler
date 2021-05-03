grammar Arithmetic;

//@header{package pp.block2.cc.antlr;}

expression
  : <assoc=right> expression POW expression
  | expression MULT expression
  | expression (PLUS | MINUS) expression
  | LEFTB expression RIGHTB
  | MINUS NUMBER
  | NUMBER
  ;


NUMBER : [1-9][0-9]* | '0';
PLUS : '+';
MINUS : '-';
MULT : '*';
POW : '^';
LEFTB : '(';
RIGHTB : ')';

// ignore whitespace
WS : [ \t\n\r] -> skip;

// everything else is a typo
TYPO : [a-zA-Z]+;
