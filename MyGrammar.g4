grammar MyGrammar;

start
    : importList (topLevelObject)* EOF
    ;

importList
    : importHeader*
    ;

importHeader
    : (IMPORT simpleIdentifier (DOT simpleIdentifier)* SEMICOLON+)
    | (FROM simpleIdentifier (DOT simpleIdentifier)* IMPORT ((importHelper (COMMA importHelper)*) | '*') SEMICOLON+)
    ;

importHelper
    : simpleIdentifier (DOT simpleIdentifier)* (ARROW simpleIdentifier)?
    ;

topLevelObject
    : classDeclaration
    | functionDeclaration
    | propertyDeclaration
    ;

classDeclaration
    : CLASS simpleIdentifier (EXTENDS simpleIdentifier)? (IMPLEMENTS simpleIdentifier (WITH simpleIdentifier)*)? classBody
    ;

classBody
    : LCURL classMemberDeclaration* RCURL
    ;

classMemberDeclaration
    : methodDeclaration
    | propertyDeclaration
    | constructorDeclaration
    ;

methodDeclaration
    : visibilityModifier? functionDeclaration
    ;

propertyDeclaration
    : visibilityModifier? variableDeclaration SEMICOLON+
    ;

constructorDeclaration
    : visibilityModifier? simpleIdentifier LPAREN functionParameters RPAREN functionBody
    ;

visibilityModifier
    : PUBLIC
    | PROTECTED
    | PRIVATE
    ;

functionDeclaration
    : (type | VOID) simpleIdentifier LPAREN functionParameters RPAREN functionBody
    ;

functionParameters
    : (parameter (COMMA parameter)*)?
    ;

parameter
    : type simpleIdentifier (ASSIGNMENT expression)?
    ;

functionBody
    : block
    ;

variableDeclaration
    : (VAR | CONST) simpleVariableDeclaration (COMMA simpleVariableDeclaration)*
    ;

simpleVariableDeclaration
    : simpleIdentifier ((COLON type (ASSIGNMENT expression)?) | (ASSIGNMENT expression))
    | simpleArrayDeclaration
    | simpleObjectDeclaration
    ;

simpleArrayDeclaration
    : (simpleIdentifier COLON NEW ARRAY LSQUARE type RSQUARE LPAREN arraySize RPAREN)
    | (simpleIdentifier ASSIGNMENT ARRAY LPAREN (expression (COMMA expression)*)? RPAREN)
    ;

arraySize
    : IntegerLiteral
    ;

simpleObjectDeclaration
    : simpleIdentifier COLON NEW simpleIdentifier LPAREN arguments RPAREN
    ;

arguments
    : (expression (COMMA expression)*)?
    ;

type
    : primitiveDataType
    | userType
    ;

primitiveDataType
    : INT
    | LONG
    | DOUBLE
    | FLOAT
    | STRING
    | CHAR
    | BOOLEAN
    ;

userType
    : simpleIdentifier
    ;

block
    : LCURL statements RCURL
    ;

statements
    : ((statement SEMICOLON+)* (conditionalExpression | exceptionHandlingExpression | loopExpression)* (statement SEMICOLON+)*)
    ;

statement
    : expression
    | declaration
    ;

declaration
    : variableDeclaration
    | functionDeclaration
    | classDeclaration
    ;

expression
    : disjunction (assignmentOperator disjunction)*
    ;

disjunction
    : conjunction (DISJ conjunction)*
    ;

conjunction
    : equalityComparison (CONJ equalityComparison)*
    ;

equalityComparison
    : comparison (equalityOperation comparison)*
    ;

comparison
    : infixFunctionCall (comparisonOperator infixFunctionCall)?
    ;

infixFunctionCall
    : additiveExpression (simpleIdentifier additiveExpression)*
    ;

additiveExpression
    : multiplicativeExpression (additiveOperator multiplicativeExpression)*
    ;

multiplicativeExpression
    : prefixUnaryExpression (multiplicativeOperation prefixUnaryExpression)*
    ;

prefixUnaryExpression
    : prefixUnaryOperation* postfixUnaryExpression
    ;

postfixUnaryExpression
    : atomicExpression postfixUnaryOperation*
    ;

atomicExpression
    : (LPAREN expression RPAREN)
    | literalConstant
    | thisExpression
    | superExpression
    | throwExpression
    | CONTINUE
    | BREAK
    | (RETURN (expression | (NEW simpleIdentifier LPAREN arguments RPAREN)))
    | simpleIdentifier
    ;

thisExpression
    : THIS DOT simpleIdentifier
    ;

callSuffix
    : LPAREN arguments RPAREN
    ;

arrayAccess
    : (LSQUARE arrayIndex RSQUARE)+
    ;

arrayIndex
    : IntegerLiteral
    ;

superExpression
    : SUPER LPAREN arguments RPAREN
    ;

literalConstant
    : BooleanLiteral
    | IntegerLiteral
    | stringLiteral
    | CharacterLiteral
    | FloatLiteral
    | DoubleLiteral
    | LongLiteral
    ;

stringLiteral
    : QUOTE (lineStringExpression | lineStringContent) QUOTE
    ;

lineStringContent
    : (~(DOLLOR | QUOTE) | (BACK_SLASH DOLLOR))+
    ;

lineStringExpression
    : lineStringContent* DOLLOR LCURL expression* RCURL lineStringContent*
    ;

loopExpression
    : forExpression
    | whileExpression
    | doWhileExpression
    ;

forExpression
    : indexBaseForExpression
    | forEachExpression
    ;

indexBaseForExpression
    : FOR LPAREN initOfForExpression? SEMICOLON conditionOfForExpression? SEMICOLON updateOfForExpression? RPAREN block
    ;

initOfForExpression
    : simpleIdentifier ASSIGNMENT expression (COMMA simpleIdentifier ASSIGNMENT expression)*
    ;

conditionOfForExpression
    : expression
    ;

updateOfForExpression
    : expression
    ;

forEachExpression
    : FOR LPAREN simpleIdentifier IN expression RPAREN block
    ;

whileExpression
    : WHILE LPAREN expression RPAREN block
    ;

doWhileExpression
    : DO block WHILE LPAREN expression RPAREN SEMICOLON+
    ;

conditionalExpression
    : ifExpression
    | switchExpression
    ;

ifExpression
    : IF LPAREN expression RPAREN block
    (ELIF LPAREN expression RPAREN block)*
    (ELSE block)?
    ;

switchExpression
    : SWITCH LPAREN expression RPAREN switchBlock
    ;

switchBlock
    : LCURL switchEntry+ defaultSwitchEntry? RCURL
    ;

switchEntry
    : CASE literalConstant COLON statements BREAK?
    ;

defaultSwitchEntry
    : DEFAULT COLON statements BREAK?
    ;

exceptionHandlingExpression
    : tryExpression catchExpression*
    ;

tryExpression
    : TRY block
    ;

catchExpression
    : (ON simpleIdentifier (CATCH LPAREN simpleIdentifier RPAREN) block)
    | (CATCH LPAREN simpleIdentifier RPAREN block)
    ;

throwExpression
    : THROW (expression | (NEW simpleIdentifier LPAREN arguments RPAREN))
    ;

assignmentOperator
    : ASSIGNMENT
    | ADD_ASSIGNMENT
    | SUB_ASSIGNMENT
    | MULT_ASSIGNMENT
    | DIV_ASSIGNMENT
    | MOD_ASSIGNMENT
    ;

equalityOperation
    : EXCL_EQ
    | EQEQ
    ;

comparisonOperator
    : LANGLE
    | RANGLE
    | LE
    | GE
    ;

additiveOperator
    : ADD | SUB
    ;

multiplicativeOperation
    : MULT
    | DIV
    | MOD
    ;

prefixUnaryOperation
    : INCR
    | DECR
    | ADD
    | SUB
    | EXCL
    | TILDE
    ;

postfixUnaryOperation
    : INCR
    | DECR
    | callSuffix
    | arrayAccess
    | DOT expression
    ;

simpleIdentifier
    : Identifier
    ;

// Lexer

COMMENT:
    '/*' .*? '*/'
    -> channel(HIDDEN);

LINE_COMMENT:
    '//' ~[\r\n]*
    -> channel(HIDDEN);


WS:
    [ \t\r\n\u000C]+
    -> channel(HIDDEN);

DOT: '.' ;
COMMA: ',' ;
LPAREN: '(' ;
RPAREN: ')' ;
LSQUARE: '[' ;
RSQUARE: ']' ;
LCURL: '{' ;
RCURL: '}' ;
MULT: '*' ;
MOD: '%' ;
DIV: '/' ;
ADD: '+' ;
SUB: '-' ;
TILDE: '~';
INCR: '++' ;
DECR: '--' ;
CONJ: '&&' ;
DISJ: '||' ;
EXCL: '!' ;
COLON: ':' ;
SEMICOLON: ';' ;
ASSIGNMENT: '=' ;
ADD_ASSIGNMENT: '+=' ;
SUB_ASSIGNMENT: '-=' ;
MULT_ASSIGNMENT: '*=' ;
DIV_ASSIGNMENT: '/=' ;
MOD_ASSIGNMENT: '%=' ;
ARROW: '=>' ;
LANGLE: '<' ;
RANGLE: '>' ;
LE: '<=' ;
GE: '>=' ;
EXCL_EQ: '!=' ;
EQEQ: '==' ;
SINGLE_QUOTE: '\'' ;
DOLLOR: '$' ;
BACK_SLASH: '\\';
QUOTE: '"' ;

// KEYWORDS
PUBLIC: 'public' ;
PRIVATE: 'private' ;
PROTECTED: 'protected' ;
IMPLEMENTS: 'implements' ;
WITH: 'with' ;
FROM: 'from' ;
IMPORT: 'import' ;
CLASS: 'class' ;
INTERFACE: 'interface' ;
VAR: 'var' ;
CONST: 'const' ;
THIS: 'this' ;
IF: 'if' ;
ELIF: 'elif' ;
ELSE: 'else' ;
SWITCH: 'switch' ;
CASE: 'case' ;
DEFAULT: 'default' ;
THROW: 'throw' ;
TRY: 'try' ;
CATCH: 'catch' ;
ON: 'on' ;
FOR: 'for' ;
DO: 'do' ;
WHILE: 'while' ;
RETURN: 'return' ;
CONTINUE: 'continue' ;
BREAK: 'break' ;
IN: 'in' ;
NEW: 'new' ;
ARRAY: 'Array' ;
EXTENDS: 'extends' ;
VOID: 'void' ;
SUPER: 'super' ;

// primitive data types
INT: 'Int' ;
LONG: 'Long' ;
DOUBLE: 'Double' ;
FLOAT: 'Float' ;
STRING: 'String' ;
CHAR: 'Char' ;
BOOLEAN: 'Boolean' ;

FloatLiteral
    : (DoubleLiteral | IntegerLiteral) [fF]
    ;


DoubleLiteral
    : IntegerLiteral
    | (IntegerLiteral DOT Digit*)
    | (DigitNoZero (DOT Digit+)? 'e' ('-' | '+') IntegerLiteral)
    ;

LongLiteral
    : IntegerLiteral 'L'
    ;

IntegerLiteral
    : '0'| (DigitNoZero Digit*)
    ;

BooleanLiteral
    : 'true'
    | 'false'
    ;

CharacterLiteral
    : '\'' . '\''
    ;

fragment Letter: [a-zA-Z];

Identifier
    : (Letter | '_' | '$') (Letter | Digit | '_' | '$')+
    ;

Digit
    : [0-9]
    ;

DigitNoZero
    : [1-9]
    ;