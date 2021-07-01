grammar NederScript;

/** Outer most nonterminal **/
program: function+;

/** Function declaration **/
function: FUN VAR LPAR (type VAR (COMMA type VAR)*)? RPAR (COLON type)? LBRACE instruction+ RBRACE;

/** Instruction **/
instruction: statement SEMI                     #normalInst
           | ifelse                             #ifelseInst
           | whileS                             #whileInst
           | forS                               #forInst
           | thread                             #threadInst
           | LBRACE (statement SEMI)+ RBRACE    #newScopeInst
           ;

/** Statement **/
statement: assign   #assignStat
         | decl     #declStat
         | returnS  #returnStat
         | funCall  #functionCallStat
         ;

/** If Else **/
ifelse: IF LPAR expr RPAR LBRACE instruction+ RBRACE (ELSE LBRACE instruction+ RBRACE)?;

/** While loops **/
whileS: WHILE LPAR expr RPAR LBRACE instruction+ RBRACE;

thread: THREAD LBRACE instruction+ RBRACE;

/** For loops **/
forS: FOR LPAR (assign | decl) SEMI expr SEMI statement RPAR LBRACE instruction+ RBRACE;

/** Assignment **/
assign: VAR (LBRACK expr RBRACK)* ASS expr;

/** Declaration **/
decl: (PUBLIC)? type VAR                            #nonTypedDecl
    | (PUBLIC)? type VAR ASS expr                   #typedDecl
    ;

/** Return statement **/
returnS: RETURN expr;

/** Function call **/
funCall: VAR LPAR (expr (COMMA expr)*)? RPAR;

/** Primitive values **/
primitive: STR                                                      #stringPrimitive
         | CHR                                                      #characterPrimitive
         | LBRACK ((primitive|VAR) (COMMA (primitive|VAR))*)? RBRACK   #arrayPrimitive
         | NUM                                                      #integerPrimitive
         | (TRUE | FALSE)                                           #booleanPrimitive
         ;

/** Type **/
type: INTEGER | BOOLEAN | CHARACTER | (ARRAY LT type GT LT NUM GT) | (STRING LT NUM GT);

/** Expressions **/
    expr: prefixOp expr                #prefixExpr
    | expr multOp expr                 #multExpr
    | expr plusOp expr                 #plusExpr
    | expr compOp expr                 #compExpr
    | expr boolOp expr                 #boolExpr
    | LPAR expr RPAR                   #parExpr
    | funCall                          #funCallExpr
    | primitive                        #primitiveExpr
    | VAR (LBRACK expr RBRACK)*   #varExpr
    ;

/** Prefix operator. */
prefixOp: MINUS | NOT;

/** Multiplicative operator. */
multOp: STAR;

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
QUOTE: '"';
SQUOTE: '\'';
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
ASS: '=';
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
PUBLIC: 'publiek';
THREAD: 'draad';

/** Types **/
INTEGER: 'Getal';
BOOLEAN: 'Booleaans';
ARRAY: 'Reeks';
STRING: 'Touw';
CHARACTER: 'Karakter';

/** Fragments for use below **/
fragment DIGIT0: [0-9];
fragment DIGIT: [1-9];
fragment LETTER: [a-zA-Z];
fragment NUMBER: DIGIT+;

/** Variables **/
VAR: LETTER (LETTER | DIGIT0 | '-' | '_')*;
NUM: (DIGIT (DIGIT0)* | DIGIT0);
STR: QUOTE .*? QUOTE;
CHR: SQUOTE (LETTER | DIGIT0) SQUOTE;
WS: [ \t\r\n]+ -> skip;
