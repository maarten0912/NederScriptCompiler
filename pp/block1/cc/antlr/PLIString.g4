lexer grammar PLIString;

STRING : '"' (~'"' | '""')* '"';