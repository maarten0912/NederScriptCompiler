grammar NederScript;

/** Outer most nonterminal **/
program: function+;

/** Function declaration **/
function: FUN VAR LPAR (type VAR (COMMA type VAR)*)? RPAR (COLON type)? LBRACE instruction+ RBRACE;

/** Instruction **/
instruction: statement SEMI             #normalInst
           | LBRACE statement+ RBRACE   #newScopeInst
           ;

/** Statement **/
statement: ifelse   #ifElseStat
         | whileS   #whileStat
         | forS     #forStat
         | assign   #assignStat
         | decl     #declStat
         | returnS  #returnStat
         | print    #printStat
         | funCall  #functionCallStat
         ;

/** If Else **/
ifelse: IF LPAR (expr | funCall) RPAR LBRACE statement+ RBRACE (ELSE LBRACE statement+ RBRACE)?;

/** While loops **/
whileS: WHILE LPAR expr RPAR LBRACE statement+ RBRACE;

/** For loops **/
forS: FOR LPAR expr SEMI expr SEMI expr RPAR LBRACE statement+ RBRACE   #forNormal
   | FOR LPAR VAR IN expr RPAR LBRACE statement+ RBRACE                 #forIn
   ;

/** Assignment **/
assign: VAR EQ expr;

/** Declaration **/
decl: (PUBLIC)? type VAR                    #nonTypedDecl
    | (PUBLIC)? type VAR EQ primitive       #typedDecl
    ;

/** Return statement **/
returnS: RETURN expr;

/** Print statement **/
print: PRINT LPAR expr RPAR;

/** Function call **/
funCall: VAR LPAR (expr | primitive) (COMMA (expr | primitive))* RPAR;

/** Primitive values **/
primitive: QUOTE .*? QUOTE                              #stringPrimitive
         | LBRACK primitive (COMMA primitive)* RBRACK   #arrayPrimitive
         | NUM                                          #integerPrimitive
         | (TRUE | FALSE)                               #booleanPrimitive
         ;

/** Type **/
type: INTEGER | BOOLEAN | ARRAY | STRING | THREAD;

/** Expressions **/
expr: prefixOp expr     #prefixExpr
    | expr multOp expr  #multExpr
    | expr plusOp expr  #plusExpr
    | expr compOp expr  #compExpr
    | expr boolOp expr  #boolExpr
    | LPAR expr RPAR    #parExpr
    | funCall           #funCallExpr
    | primitive         #primitiveExpr
    | VAR               #idExpr
    ;

/** Prefix operator. */
prefixOp: MINUS | NOT;

/** Multiplicative operator. */
multOp: STAR | SLASH;

/** Additive operator. */
plusOp: PLUS | MINUS;

/** Boolean operator. */
boolOp: AND | OR;

/** Comparison operator. */
compOp: LE | LT | GE | GT | EQ | NE;

/** Code structure tokens **/
LPAR: '(';
RPAR: ')';
LBRACE: '{';
RBRACE: '}';
LBRACK: '[';
RBRACK: ']';
COLON: ':';
SEMI: ';';
QUOTE: '"' | '\'';
COMMA: ',';

/** Expression tokens **/
MINUS: '-';
PLUS: '+';
NOT: '!';
STAR: '*';
SLASH: '/';
AND: '&&';
OR: '||';
LE: '<=';
LT: '<';
GE: '>=';
GT: '>';
EQ: '==';
NE: '!=';

/** Keywords tokens **/
WHILE: 'terwijl';
FOR: 'voor';
IN: 'in';
IF : 'als';
ELSE: 'anders';
TRUE: 'waar';
FALSE: 'onwaar';
FUN: 'functie';
RETURN: 'geefterug';
PRINT: 'afdrukken';
PUBLIC: 'public';

/** Types **/
INTEGER: 'Getal';
BOOLEAN: 'Booleaans';
ARRAY: 'Reeks';
STRING: 'Touw';
THREAD: 'Draad';

/** Fragments for use below **/
fragment DIGIT0: [0-9];
fragment DIGIT: [1-9];
fragment LETTER: [a-zA-Z];
fragment NUMBER: DIGIT+;

/** Variables **/
VAR: LETTER (LETTER | DIGIT0)*;
NUM: DIGIT (DIGIT0)*;
WS: [ \t\r\n]+ -> skip;
