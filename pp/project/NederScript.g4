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
           | LBRACE (statement SEMI)+ RBRACE    #newScopeInst
           ;

/** Statement **/
statement: assign   #assignStat
         | decl     #declStat
         | returnS  #returnStat
         | print    #printStat
         | funCall  #functionCallStat
         ;

/** If Else **/
ifelse: IF LPAR (expr | funCall) RPAR LBRACE instruction+ RBRACE (ELSE LBRACE instruction+ RBRACE)?;

/** While loops **/
whileS: WHILE LPAR expr RPAR LBRACE instruction+ RBRACE;

/** For loops **/
forS: FOR LPAR statement SEMI expr SEMI statement RPAR LBRACE instruction+ RBRACE   #forNormal
   | FOR LPAR type VAR IN expr RPAR LBRACE instruction+ RBRACE            #forIn
   ;

/** Assignment **/
assign: VAR (LBRACK (NUM|VAR) RBRACK)* ASS expr;

/** Declaration **/
decl: (PUBLIC)? type VAR                            #nonTypedDecl
    | (PUBLIC)? type VAR ASS (primitive | funCall)  #typedDecl
    ;

/** Return statement **/
returnS: RETURN expr;

/** Print statement **/
print: PRINT LPAR expr RPAR;

/** Function call **/
funCall: VAR LPAR (expr | primitive) (COMMA (expr | primitive))* RPAR;

/** Primitive values **/
primitive: STR                                          #stringPrimitive
         | CHR                                          #characterPrimitive
         | LBRACK primitive (COMMA primitive)* RBRACK   #arrayPrimitive
         | NUM                                          #integerPrimitive
         | (TRUE | FALSE)                               #booleanPrimitive
         ;

/** Type **/
type: INTEGER | BOOLEAN | ARRAY | STRING | THREAD;

/** Expressions **/
    expr: prefixOp expr                #prefixExpr
    | expr multOp expr                 #multExpr
    | expr plusOp expr                 #plusExpr
    | expr compOp expr                 #compExpr
    | expr boolOp expr                 #boolExpr
    | LPAR expr RPAR                   #parExpr
    | funCall                          #funCallExpr
    | primitive                        #primitiveExpr
    | VAR (LBRACK (VAR|NUM) RBRACK)*   #varExpr
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
PRINT: 'afdrukken';
PUBLIC: 'publiek';

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
VAR: LETTER (LETTER | DIGIT0 | '-' | '_')*;
NUM: (DIGIT (DIGIT0)* | DIGIT0);
STR: QUOTE .*? QUOTE;
CHR: SQUOTE (LETTER | DIGIT0) SQUOTE;
WS: [ \t\r\n]+ -> skip;
