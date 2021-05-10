lexer grammar SimpleArithmeticVocab;

//@header{package pp.block3.cc.antlr;}

HAT  : '^';
PLUS   : '+';
EQUALS  : '=';
LPAR   : '(';
RPAR   : ')';

NUMBER : [0-9]+;
BOOL : 'true' | 'false';
STR : [a-zA-Z]+;

// ignore whitespace
WS : [ \t\n\r] -> skip;
