parser grammar RubyParser;

options {
    tokenVocab = RubyLexer;
}

// --------------------------------------------------------
// Program
// --------------------------------------------------------

program
    :   compoundStatement EOF
    ;

compoundStatement
    :   statements? (SEMI | NL)*
    ;

// --------------------------------------------------------
// Statements
// --------------------------------------------------------

statements
    :   (SEMI | NL)*  statement ((SEMI | NL)+ statement)*
    ;

statement
    :   expression
    |   ALIAS NL* definedMethodNameOrSymbol definedMethodNameOrSymbol
    |   UNDEF NL* definedMethodNameOrSymbol (COMMA NL* definedMethodNameOrSymbol)*
    |   statement statementModifier NL* expression
    |   singleAssignmentStatement
    |   multipleAssignmentStatement
    ;

definedMethodNameOrSymbol
    :   definedMethodName
    |   symbol
    ;


statementModifier
    :   IF
    |   UNLESS
    |   WHILE
    |   UNTIL
    |   RESCUE
    ;

// --------------------------------------------------------
// Expressions
// --------------------------------------------------------

expression
    :   operatorExpression
    |   EMARK? methodInvocationWithoutParentheses
    |   NOT NL* expression
    |   expression (AND|OR) NL* expression
    ;

expressionOrCommand
    :   expression
    ;

// --------------------------------------------------------
// Method invocation expressions
// --------------------------------------------------------

methodIdentifier
    :   LOCAL_VARIABLE_IDENTIFIER
    |   CONSTANT_IDENTIFIER
    |   methodOnlyIdentifier
    ;

methodName
    :   methodIdentifier
    |   keyword
    |   pseudoVariable
    ;

methodOnlyIdentifier
    :   (CONSTANT_IDENTIFIER | LOCAL_VARIABLE_IDENTIFIER | pseudoVariable) (EMARK | QMARK)
    ;
    
methodInvocationWithoutParentheses
    :   command
    |   chainedCommandWithDoBlock ((DOT | COLON2) methodName commandArgumentList)?
    |   RETURN primaryValueList
    |   BREAK primaryValueList
    |   NEXT primaryValueList
    |   YIELD primaryValueList
    ;

command
    :   primary NL? (AMPDOT | DOT | COLON2) methodName (command | commandArgumentList)
    |   methodIdentifier (command | commandArgumentList)
    ;

chainedCommandWithDoBlock
    :   commandWithDoBlock chainedMethodInvocation*
    ;

chainedMethodInvocation
    :   (DOT | COLON2) methodName argumentWithParentheses?
    ;

commandWithDoBlock
    :   SUPER argumentList doBlock
    |   methodIdentifier argumentList doBlock
    |   primary (DOT | COLON2) methodName argumentList doBlock
    ;

// --------------------------------------------------------
// Method arguments
// --------------------------------------------------------

indexingArgumentList
    :   command
    |   operatorExpressionList COMMA?
    |   operatorExpressionList COMMA splattingArgument
    |   associationList COMMA?
    |   splattingArgument
    ;

splattingArgument
    :   STAR operatorExpression
    |   STAR2 operatorExpression
    ;

operatorExpressionList
    :   operatorExpression (COMMA NL* operatorExpression)*
    ;
    
operatorExpressionList2
    :   operatorExpression (COMMA NL* operatorExpression)+
    ;

argumentWithParentheses
    :   LPAREN NL* COMMA? NL* RPAREN
    |   LPAREN NL* argumentList COMMA? NL* RPAREN
    |   LPAREN NL* operatorExpressionList COMMA NL* chainedCommandWithDoBlock COMMA? NL* RPAREN
    |   LPAREN NL* chainedCommandWithDoBlock COMMA? NL* RPAREN
    ;

argumentList
    :   blockArgument
    |   splattingArgument (COMMA NL* blockArgument)?
    |   operatorExpressionList (COMMA NL* associationList)? (COMMA NL* splattingArgument)? (COMMA NL* blockArgument)?
    |   associationList (COMMA NL* splattingArgument)? (COMMA NL* blockArgument)?
    |   command
    ;
    
commandArgumentList
    :   associationList
    |   primaryValueList (COMMA NL* associationList)?
    ;    

primaryValueList
    :   primaryValue (COMMA NL* primaryValue)*
    ;

blockArgument
    :   AMP operatorExpression
    ;
    
// --------------------------------------------------------
// Blocks
// --------------------------------------------------------

block
    :   LCURLY NL* blockParameter? compoundStatement RCURLY
    |   doBlock
    ;

doBlock
    :   DO NL* blockParameter? bodyStatement END
    ;

blockParameter
    :   BAR NL* BAR
    |   BAR NL* blockParameterList NL* BAR
    ;

blockParameterList
    :   leftHandSide
    |   multipleLeftHandSide
    ;

// --------------------------------------------------------
// Operator expression
// --------------------------------------------------------
operatorExpression
    :   primary
   
    //|   rangeOperator NL* operatorExpression
    |   operatorExpression QMARK NL* operatorExpression NL* COLON NL* operatorExpression
    ;

unaryOperator
    :   TILDE
    |   PLUS
    |   EMARK
    ;

multiplicativeOperator
    :   STAR
    |   SLASH
    |   PERCENT
    ;

additiveOperator
    :   PLUS
    |   MINUS
    ;

bitwiseShiftOperator
    :   LT2
    |   GT2
    ;

bitwiseOrOperator
    :   BAR
    |   CARET
    ;

relationalOperator
    :   GT
    |   GTEQ
    |   LT
    |   LTEQ
    ;

equalityOperator
    :   LTEQGT
    |   EQ2
    |   EQ3
    |   EMARKEQ
    |   EQTILDE
    |   EMARKTILDE
    ;

rangeOperator
    :   DOT2
    |   DOT3
    ;
    
// --------------------------------------------------------
// Single assignments
// --------------------------------------------------------

assignmentOperator
    :   EQ
    |   ASSIGNMENT_OPERATOR
    ;

singleAssignmentStatement
    :   variable assignmentOperator NL* methodInvocationWithoutParentheses
    |   COLON2 CONSTANT_IDENTIFIER assignmentOperator NL* methodInvocationWithoutParentheses
    |   primary LBRACK indexingArgumentList? RBRACK assignmentOperator NL* methodInvocationWithoutParentheses
    |   primary (DOT | COLON2) methodName assignmentOperator NL* methodInvocationWithoutParentheses
    ;

// --------------------------------------------------------
// Multiple assignments
// --------------------------------------------------------

multipleAssignmentStatement
    :   leftHandSide EQ NL* multipleRightHandSide
    |   packingLeftHandSide EQ NL* (methodInvocationWithoutParentheses | operatorExpression)
    |   multipleLeftHandSide EQ NL* multipleRightHandSide
    |   multipleLeftHandSideExceptPacking EQ NL* (methodInvocationWithoutParentheses | operatorExpression)
    ;

leftHandSide
    :   variable (EQ primary)?
    |   primary LBRACK indexingArgumentList? RBRACK
    |   primary (DOT | COLON2) (LOCAL_VARIABLE_IDENTIFIER | CONSTANT_IDENTIFIER)
    |   COLON2 CONSTANT_IDENTIFIER
    ;

multipleLeftHandSide
    :   (multipleLeftHandSideItem COMMA)+ multipleLeftHandSideItem?
    |   (multipleLeftHandSideItem COMMA)+ packingLeftHandSide? (COMMA? NL* procParameter)? COMMA?
    |   packingLeftHandSide
    |   groupedLeftHandSide
    ;

multipleLeftHandSideExceptPacking
    :   (multipleLeftHandSideItem COMMA)+ multipleLeftHandSideItem?
    |   (multipleLeftHandSideItem COMMA)+ packingLeftHandSide?
    |   groupedLeftHandSide
    ;

packingLeftHandSide
    :   STAR leftHandSide?
    ;

groupedLeftHandSide
    :   LPAREN multipleLeftHandSide RPAREN
    ;

multipleLeftHandSideItem
    :   leftHandSide
    |   groupedLeftHandSide
    ;

multipleRightHandSide
    :   operatorExpressionList2 (COMMA splattingRightHandSide)?
    |   splattingRightHandSide
    ;
     
splattingRightHandSide
    :   splattingArgument
    ;

// --------------------------------------------------------
// Primary expressions
// --------------------------------------------------------

primaryValue
    :   variable assignmentOperator NL* operatorExpression
    |   primaryValue (DOT | COLON2) methodName assignmentOperator NL* operatorExpression
    |   COLON2 CONSTANT_IDENTIFIER assignmentOperator NL* operatorExpression
    |   primaryValue LBRACK indexingArgumentList? RBRACK assignmentOperator NL* operatorExpression
    |   primaryValue assignmentOperator NL* operatorExpression RESCUE operatorExpression
    |   IF NL* (command | primaryValue) thenClause elsifClause* elseClause? END
    |   UNLESS NL* (command | primaryValue) thenClause elseClause? END
    |   UNTIL NL* (command | primaryValue) doClause END
    |   CLASS classPath (LT expression)? (SEMI | NL) bodyStatement END
    |   CLASS LT2 expression (SEMI | NL) bodyStatement END
    |   MODULE modulePath bodyStatement END
    |   DEF definedMethodName methodParameterPart methodBody END
    |   DEF singletonObject (DOT | COLON2) definedMethodName methodParameterPart methodBody END
    |   DEF definedMethodName (LPAREN parameterList? RPAREN)? EQ NL* expression
    |   MINUSGT (LPAREN parameterList? RPAREN)? block
    |   YIELD argumentWithParentheses?
    |   SUPER argumentWithParentheses? block?
    |   SUPER argumentList? block?
    |   BEGIN bodyStatement END
    |   CASE NL* expression (SEMI | NL)* whenClause+ elseClause? END
    |   CASE (SEMI | NL)* whenClause+ elseClause? END
    |   WHILE NL* expression doClause END
    |   FOR NL* forVariable IN NL* expression doClause END
    |   variableReference
    |   COLON2 CONSTANT_IDENTIFIER
    |   LBRACK NL* indexingArgumentList? NL* RBRACK
    |   QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START quotedNonExpandedArrayElementList? QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END
    |   QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_START quotedNonExpandedArrayElementList? QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END
    |   QUOTED_EXPANDED_STRING_ARRAY_LITERAL_START quotedExpandedArrayElementList? QUOTED_EXPANDED_STRING_ARRAY_LITERAL_END
    |   QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_START quotedExpandedArrayElementList? QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_END
    |   LCURLY NL* (associationList COMMA?)? NL* RCURLY
    |   (PLUS | MINUS)? unsignedNumericLiteral
    |   singleQuotedString (singleQuotedString | doubleQuotedString)*
    |   doubleQuotedString (doubleQuotedString | singleQuotedString)*
    |   quotedNonExpandedLiteralString
    |   quotedExpandedLiteralString
    |   quotedExpandedExternalCommandString
    |   symbol
    |   REGULAR_EXPRESSION_START regexpLiteralContent* REGULAR_EXPRESSION_END
    |   QUOTED_EXPANDED_REGULAR_EXPRESSION_START quotedExpandedLiteralStringContent* QUOTED_EXPANDED_REGULAR_EXPRESSION_END
    |   IS_DEFINED LPAREN expression RPAREN
    |   methodOnlyIdentifier
    |   methodIdentifier block
    |   methodIdentifier argumentWithParentheses block?
    |   LPAREN compoundStatement RPAREN
    |   IS_DEFINED primaryValue
    |   primaryValue LBRACK indexingArgumentList? RBRACK
    |   primaryValue NL* (AMPDOT | DOT | COLON2) NL* methodName argumentWithParentheses? block?
    |   unaryOperator primaryValue
    |   primaryValue powerOperator=STAR2    NL* primaryValue
    |   MINUS primaryValue
    |   primaryValue multiplicativeOperator NL* primaryValue
    |   primaryValue additiveOperator       NL* primaryValue
    |   primaryValue bitwiseShiftOperator   NL* primaryValue
    |   primaryValue bitwiseAndOperator=AMP NL* primaryValue
    |   primaryValue bitwiseOrOperator      NL* primaryValue
    |   primaryValue relationalOperator     NL* primaryValue
    |   primaryValue equalityOperator       NL* primaryValue
    |   primaryValue andOperator=AMP2       NL* primaryValue
    |   primaryValue orOperator=BAR2        NL* primaryValue
    |   primaryValue rangeOperator          NL* primaryValue
    |   NOT primaryValue
    |   primaryValue (AND|OR) NL* primaryValue
    ;

primary
    :   (RETURN | BREAK | NEXT | REDO | RETRY)
    |   primaryValue
    ;

// --------------------------------------------------------
// if expression
// --------------------------------------------------------

thenClause
    :   (SEMI | NL)+ compoundStatement
    |   (SEMI | NL)? THEN compoundStatement
    ;

elseClause
    :   ELSE compoundStatement
    ;

elsifClause
    :   ELSIF NL* expression thenClause
    ;

// --------------------------------------------------------
// case expression
// --------------------------------------------------------

whenClause
    :   WHEN NL* whenArgument thenClause
    ;

whenArgument
    :   operatorExpressionList (COMMA splattingArgument)?
    |   splattingArgument
    ;

// --------------------------------------------------------
// while expression
// --------------------------------------------------------

doClause
    :   (SEMI | NL)+ compoundStatement
    |   DO compoundStatement
    ;

// --------------------------------------------------------
// for expression
// --------------------------------------------------------

forVariable
    :   leftHandSide
    |   multipleLeftHandSide
    ;

// --------------------------------------------------------
// begin expression
// --------------------------------------------------------

bodyStatement
    :   compoundStatement rescueClause* elseClause? ensureClause?
    ;

rescueClause
    :   RESCUE exceptionClassList? exceptionVariableAssignment? thenClause
    ;

exceptionClassList
    :   operatorExpression
    |   multipleRightHandSide
    ;

exceptionVariableAssignment
    :   EQGT leftHandSide
    ;
    
ensureClause
    :   ENSURE compoundStatement
    ;

// --------------------------------------------------------
// Variable references
// --------------------------------------------------------

variableReference
    :   variable
    |   pseudoVariable
    ;

variable
    :   CONSTANT_IDENTIFIER
    |   GLOBAL_VARIABLE_IDENTIFIER
    |   CLASS_VARIABLE_IDENTIFIER
    |   INSTANCE_VARIABLE_IDENTIFIER
    |   LOCAL_VARIABLE_IDENTIFIER
    ;


// --------------------------------------------------------
// Pseudo variables
// --------------------------------------------------------

pseudoVariable
    :   NIL
    |   TRUE
    |   FALSE
    |   SELF
    |   LINE__
    |   FILE__
    |   ENCODING__
    ;

// --------------------------------------------------------
// Hash constructor
// --------------------------------------------------------

associationList
    :   association (COMMA NL* association)*
    ;
    
association
    :   (operatorExpression|keyword) (EQGT | COLON) NL* operatorExpression
    ;

// --------------------------------------------------------
// Method definitions
// --------------------------------------------------------

definedMethodName
    :   methodName
    |   ASSIGNMENT_LIKE_METHOD_IDENTIFIER
    |   LBRACK RBRACK EQ?
    |   EQ2
    |   EQ3
    |   LTEQGT
    ;

methodBody
    :   bodyStatement
    ;

// --------------------------------------------------------
// Method parameters
// --------------------------------------------------------

methodParameterPart
    :   LPAREN NL* parameterList? NL* RPAREN
    |   parameterList? (SEMI | NL)
    ;

parameterList
    :   mandatoryOrOptionalParameterList (COMMA NL* arrayParameter)? (COMMA NL* hashParameter)? (COMMA NL* procParameter)?
    |   arrayParameter (COMMA NL* hashParameter)? (COMMA NL* procParameter)?
    |   hashParameter (COMMA NL* procParameter)?
    |   procParameter
    ;

mandatoryOrOptionalParameterList
    :   (mandatoryParameter | optionalParameter) (COMMA NL* (mandatoryParameter | optionalParameter))*
    ;

mandatoryParameter
    :   LOCAL_VARIABLE_IDENTIFIER COLON?
    ;

optionalParameter
    :   optionalParameterName (EQ|COLON) NL* operatorExpression
    ;

optionalParameterName
    :   LOCAL_VARIABLE_IDENTIFIER
    ;

arrayParameter
    :   STAR LOCAL_VARIABLE_IDENTIFIER?
    ;

hashParameter
    :   STAR2 LOCAL_VARIABLE_IDENTIFIER?
    ;

procParameter
    :   AMP procParameterName
    ;

procParameterName
    :   LOCAL_VARIABLE_IDENTIFIER
    ;

// --------------------------------------------------------
// Class definition
// --------------------------------------------------------

classPath
    :   topClassPath
    |   className
    |   classPath COLON2 className
    ;

className
    :   CONSTANT_IDENTIFIER
    ;

topClassPath
    :   COLON2 className
    ;

// --------------------------------------------------------
// Singleton method definition
// --------------------------------------------------------

singletonObject
    :   variableReference
    |   LPAREN expression RPAREN
    ;

// --------------------------------------------------------
// Module definition
// --------------------------------------------------------

modulePath
    :   topModulePath
    |   moduleName
    |   modulePath COLON2 moduleName
    ;

moduleName
    :   CONSTANT_IDENTIFIER
    ;

topModulePath
    :   COLON2 moduleName
    ;

// --------------------------------------------------------
// Regex literals
// --------------------------------------------------------

regexpLiteralContent
    :   REGULAR_EXPRESSION_BODY
    |   REGULAR_EXPRESSION_INTERPOLATION_BEGIN compoundStatement REGULAR_EXPRESSION_INTERPOLATION_END
    ;

// --------------------------------------------------------
// String literals
// --------------------------------------------------------

singleQuotedString
    :   SINGLE_QUOTED_STRING_LITERAL
    ;

doubleQuotedString
    :   DOUBLE_QUOTED_STRING_START doubleQuotedStringContent* DOUBLE_QUOTED_STRING_END
    ;

quotedExpandedExternalCommandString
    :   QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_START 
        quotedExpandedLiteralStringContent*
        QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_END
    ;

doubleQuotedStringContent
    :   DOUBLE_QUOTED_STRING_CHARACTER_SEQUENCE
    |   STRING_INTERPOLATION_BEGIN compoundStatement STRING_INTERPOLATION_END
    ;

quotedNonExpandedLiteralString
    :   QUOTED_NON_EXPANDED_STRING_LITERAL_START NON_EXPANDED_LITERAL_CHARACTER_SEQUENCE? QUOTED_NON_EXPANDED_STRING_LITERAL_END
    ;

quotedExpandedLiteralString
    :   QUOTED_EXPANDED_STRING_LITERAL_START quotedExpandedLiteralStringContent* QUOTED_EXPANDED_STRING_LITERAL_END
    ;

quotedExpandedLiteralStringContent
    :   EXPANDED_LITERAL_CHARACTER_SEQUENCE
    |   DELIMITED_STRING_INTERPOLATION_BEGIN compoundStatement DELIMITED_STRING_INTERPOLATION_END
    ;

// --------------------------------------------------------
// Array literals
// --------------------------------------------------------

quotedNonExpandedArrayElementContent
    :   NON_EXPANDED_ARRAY_ITEM_CHARACTER+
    ;
    
quotedExpandedArrayElementContent
    :   EXPANDED_ARRAY_ITEM_CHARACTER
    |   DELIMITED_ARRAY_ITEM_INTERPOLATION_BEGIN compoundStatement DELIMITED_ARRAY_ITEM_INTERPOLATION_END
    ;

quotedExpandedArrayElement
    :   quotedExpandedArrayElementContent+
    ;

quotedNonExpandedArrayElementList
    :   NON_EXPANDED_ARRAY_ITEM_SEPARATOR*
        quotedNonExpandedArrayElementContent 
        (NON_EXPANDED_ARRAY_ITEM_SEPARATOR+ quotedNonExpandedArrayElementContent)*
        NON_EXPANDED_ARRAY_ITEM_SEPARATOR*
    ;

quotedExpandedArrayElementList
    :   EXPANDED_ARRAY_ITEM_SEPARATOR*
        quotedExpandedArrayElement
        (EXPANDED_ARRAY_ITEM_SEPARATOR+ quotedExpandedArrayElement)*
        EXPANDED_ARRAY_ITEM_SEPARATOR*
    ;


// --------------------------------------------------------
// Symbol literals
// --------------------------------------------------------

symbol
    :   SYMBOL_LITERAL
    |   COLON singleQuotedString
    |   COLON doubleQuotedString
    ;

// --------------------------------------------------------
// Numeric literals
// --------------------------------------------------------

unsignedNumericLiteral
    :   DECIMAL_INTEGER_LITERAL
    |   BINARY_INTEGER_LITERAL
    |   OCTAL_INTEGER_LITERAL
    |   HEXADECIMAL_INTEGER_LITERAL
    |   FLOAT_LITERAL_WITHOUT_EXPONENT
    |   FLOAT_LITERAL_WITH_EXPONENT
    ;

// --------------------------------------------------------
// Commons
// --------------------------------------------------------

keyword
    :   BEGIN_
    |   END_
    |   ALIAS
    |   AND
    |   BEGIN
    |   BREAK
    |   CASE
    |   CLASS
    |   DEF
    |   IS_DEFINED
    |   DO
    |   ELSE
    |   ELSIF
    |   END
    |   ENSURE
    |   FOR
    |   IF
    |   IN
    |   MODULE
    |   NEXT
    |   NOT
    |   OR
    |   REDO
    |   RESCUE
    |   RETRY
    |   RETURN
    |   SUPER
    |   THEN
    |   UNDEF
    |   UNLESS
    |   UNTIL
    |   WHEN
    |   WHILE
    |   YIELD
    ;