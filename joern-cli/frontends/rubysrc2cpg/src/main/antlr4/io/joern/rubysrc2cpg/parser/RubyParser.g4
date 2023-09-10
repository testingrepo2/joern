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

statements
    :   (SEMI | NL)*  statement ((SEMI | NL)+ statement)*
    ;

statement
    :   expressionOrCommand
        # expressionOrCommandStatement
    |   ALIAS NL* oldName=definedMethodNameOrSymbol newName=definedMethodNameOrSymbol
        # aliasStatement
    |   UNDEF NL* definedMethodNameOrSymbol (COMMA NL* definedMethodNameOrSymbol)*
        # undefStatement
    |   statement statementModifier NL* expressionOrCommand
        # modifierStatement
    |   singleAssignmentStatement
        # singleAssignmentStatementStatement
    |   multipleAssignmentStatement
        # multipleAssignmentStatementStatement
    ;

definedMethodNameOrSymbol
    :   definedMethodName
    |   symbol
    ;

singleAssignmentStatement
    :   variable assignmentOperator NL* methodInvocationWithoutParentheses
    |   COLON2 CONSTANT_IDENTIFIER assignmentOperator NL* methodInvocationWithoutParentheses
    |   primary LBRACK indexingArgumentList? RBRACK assignmentOperator NL* methodInvocationWithoutParentheses
    |   primary (DOT | COLON2) methodName assignmentOperator NL* methodInvocationWithoutParentheses
    ;

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
    :   primary NL? (AMPDOT | DOT | COLON2) methodName commandArgument
    |   methodIdentifier commandArgument
    ;

commandArgument
    :   commandArgumentList
    |   command
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
// Expressions
// --------------------------------------------------------

expressionOrCommand
    :   operatorExpression
        # operatorExpressionOrCommand
    |   EMARK? methodInvocationWithoutParentheses
        # commandExpressionOrCommand
    |   NOT NL* expressionOrCommand
        # notExpressionOrCommand
    |   lhs=expressionOrCommand binOp=(AND|OR) NL* rhs=expressionOrCommand
        # keywordAndOrExpressionOrCommand
    ;

operatorExpression
    :   primary
        # primaryOperatorExpression
    |   operatorExpression QMARK NL* operatorExpression NL* COLON NL* operatorExpression
        # ternaryOperatorExpression
    ;

primary
    :   RETURN
        # returnWithoutArguments
    |   BREAK
        # breakWithoutArguments
    |   NEXT
        # nextWithoutArguments
    |   REDO
        # redoWithoutArguments
    |   RETRY
        # retryWithoutArguments
    |   primaryValue
        # primaryValuePrimary
    ;

primaryValue
    :   // Assignment expressions
        lhs=variable assignmentOperator NL* rhs=operatorExpression
        # localVariableAssignmentExpression
    |   primaryValue (DOT | COLON2) methodName assignmentOperator NL* operatorExpression
        # attributeAssignmentExpression
    |   COLON2 CONSTANT_IDENTIFIER assignmentOperator NL* operatorExpression
        # constantAssignmentExpression
    |   primaryValue LBRACK indexingArgumentList? RBRACK assignmentOperator NL* operatorExpression
        # bracketAssignmentExpression
    |   primaryValue assignmentOperator NL* operatorExpression RESCUE operatorExpression
        # assignmentWithRescue
        
        // Definitions
    |   CLASS classPath (LT commandOrPrimaryValue)? (SEMI | NL) bodyStatement END
        # classDefinition
    |   CLASS LT2 commandOrPrimaryValue (SEMI | NL) bodyStatement END
        # singletonClassDefinition
    |   MODULE modulePath bodyStatement END
        # moduleDefinition
    |   DEF definedMethodName methodParameterPart bodyStatement END
        # methodDefinition
    |   DEF singletonObject (DOT | COLON2) definedMethodName methodParameterPart bodyStatement END
        # singletonMethodDefinition
    |   DEF definedMethodName (LPAREN parameterList? RPAREN)? EQ NL* commandOrPrimaryValue
        # endlessMethodDefinition
    |   MINUSGT (LPAREN parameterList? RPAREN)? block
        # lambdaExpression

        // Control structures
    |   IF NL* commandOrPrimaryValue thenClause elsifClause* elseClause? END
        # ifExpression
    |   UNLESS NL* commandOrPrimaryValue thenClause elseClause? END
        # unlessExpression
    |   UNTIL NL* commandOrPrimaryValue doClause END
        # untilExpression
    |   YIELD argumentWithParentheses?
        # yieldExpression
    |   BEGIN bodyStatement END
        # beginEndExpression
    |   CASE NL* commandOrPrimaryValue (SEMI | NL)* whenClause+ elseClause? END
        # caseWithExpression
    |   CASE (SEMI | NL)* whenClause+ elseClause? END
        # caseWithoutExpression
    |   WHILE NL* commandOrPrimaryValue doClause END
        # whileExpression
    |   FOR NL* forVariable IN NL* commandOrPrimaryValue doClause END
        # forExpression
    
        // Non-nested calls
    |   SUPER argumentWithParentheses? block?
        # superWithParentheses
    |   SUPER argumentList? block?
        # superWithoutParentheses
    |   IS_DEFINED LPAREN expressionOrCommand RPAREN
        # isDefinedExpression
    |   IS_DEFINED primaryValue
        # isDefinedCommand
    |   methodOnlyIdentifier
        # methodCallExpression
    |   methodIdentifier block
        # methodCallWithBlockExpression
    |   methodIdentifier argumentWithParentheses block?
        # methodCallWithParenthesesExpression
    |   variableReference
        # methodCallOrVariableReference
        
        // Literals
    |   LBRACK NL* indexingArgumentList? NL* RBRACK
        # bracketedArrayLiteral
    |   QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_START quotedNonExpandedArrayElementList? QUOTED_NON_EXPANDED_STRING_ARRAY_LITERAL_END
        # quotedNonExpandeddStringArrayLiteral
    |   QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_START quotedNonExpandedArrayElementList? QUOTED_NON_EXPANDED_SYMBOL_ARRAY_LITERAL_END
        # quotedNonExpandedSymbolArrayLiteral
    |   QUOTED_EXPANDED_STRING_ARRAY_LITERAL_START quotedExpandedArrayElementList? QUOTED_EXPANDED_STRING_ARRAY_LITERAL_END
        # quotedExpandedStringArrayLiteral
    |   QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_START quotedExpandedArrayElementList? QUOTED_EXPANDED_SYMBOL_ARRAY_LITERAL_END
        # quotedExpandedSymbolArrayLiteral
    |   LCURLY NL* (associationList COMMA?)? NL* RCURLY
        # hashLiteral
    |   (PLUS | MINUS)? unsignedNumericLiteral
        # numericLiteral
    |   singleQuotedString (singleQuotedString | doubleQuotedString)*
        # singleQuotedStringExpression
    |   doubleQuotedString (doubleQuotedString | singleQuotedString)*
        # doubleQuotedStringExpression
    |   QUOTED_NON_EXPANDED_STRING_LITERAL_START NON_EXPANDED_LITERAL_CHARACTER_SEQUENCE? QUOTED_NON_EXPANDED_STRING_LITERAL_END
        # quotedNonExpandedStringLiteral
    |   QUOTED_EXPANDED_STRING_LITERAL_START quotedExpandedLiteralStringContent* QUOTED_EXPANDED_STRING_LITERAL_END
        # quotedExpandedStringLiteral
    |   QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_START quotedExpandedLiteralStringContent* QUOTED_EXPANDED_EXTERNAL_COMMAND_LITERAL_END
        # quotedExpandedExternalCommandLiteral
    |   symbol
        # symbolExpression
    |   REGULAR_EXPRESSION_START regexpLiteralContent* REGULAR_EXPRESSION_END
        # regularExpressionLiteral
    |   QUOTED_EXPANDED_REGULAR_EXPRESSION_START quotedExpandedLiteralStringContent* QUOTED_EXPANDED_REGULAR_EXPRESSION_END
        # quotedExpandedRegularExpressionLiteral
    
    |   LPAREN compoundStatement RPAREN
        # groupingStatement
    
        // Member accesses
    |   primaryValue LBRACK indexingArgumentList? RBRACK
        # indexingAccessExpression
    |   primaryValue NL* (AMPDOT | DOT | COLON2) NL* methodName argumentWithParentheses? block?
        # memberAccessExpression
    
        // Unary and binary expressions
    |   unaryOperator primaryValue
        # unaryExpression
    |   <assoc=right> primaryValue powerOperator=STAR2    NL* primaryValue
        # powerExpression
    |   MINUS primaryValue
        # unaryMinusExpression
    |   primaryValue multiplicativeOperator NL* primaryValue
        # multiplicativeExpression
    |   primaryValue additiveOperator       NL* primaryValue
        # additiveExpression
    |   primaryValue bitwiseShiftOperator   NL* primaryValue
        # shiftExpression
    |   primaryValue bitwiseAndOperator=AMP NL* primaryValue
        # bitwiseAndExpression
    |   primaryValue bitwiseOrOperator      NL* primaryValue
        # bitwiseOrExpression
    |   primaryValue relationalOperator     NL* primaryValue
        # relationalExpression
    |   primaryValue equalityOperator       NL* primaryValue
        # equalityExpression
    |   primaryValue andOperator=AMP2       NL* primaryValue
        # logicalAndExpression
    |   primaryValue orOperator=BAR2        NL* primaryValue
        # logicalOrExpression
    |   primaryValue rangeOperator          NL* primaryValue
        # rangeExpression
    ;

commandOrPrimaryValue
    :   command
        # commandCommandOrPrimaryValue
    |   NOT commandOrPrimaryValue
        # notCommandOrPrimaryValue
    |   commandOrPrimaryValue (AND|OR) NL* commandOrPrimaryValue
        # keywordAndOrCommandOrPrimaryValue
    |   primaryValue
        # primaryValueCommandOrPrimaryValue
    ;

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

thenClause
    :   (SEMI | NL)+ compoundStatement
    |   (SEMI | NL)? THEN compoundStatement
    ;

elseClause
    :   ELSE compoundStatement
    ;

elsifClause
    :   ELSIF NL* expressionOrCommand thenClause
    ;

whenClause
    :   WHEN NL* whenArgument thenClause
    ;

whenArgument
    :   operatorExpressionList (COMMA splattingArgument)?
    |   splattingArgument
    ;

doClause
    :   (SEMI | NL)+ compoundStatement
    |   DO compoundStatement
    ;

forVariable
    :   leftHandSide
    |   multipleLeftHandSide
    ;

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

definedMethodName
    :   methodName
    |   ASSIGNMENT_LIKE_METHOD_IDENTIFIER
    |   LBRACK RBRACK EQ?
    |   EQ2
    |   EQ3
    |   LTEQGT
    ;

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

singletonObject
    :   variableReference
    |   LPAREN expressionOrCommand RPAREN
    ;

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

variableReference
    :   variable
        # variableVariableReference
    |   pseudoVariable
        # pseudoVariableVariableReference
    |   COLON2 CONSTANT_IDENTIFIER
        # constantVariableReference
    ;

associationList
    :   association (COMMA NL* association)*
    ;
    
association
    :   associationKey (EQGT | COLON) NL* operatorExpression
    ;
    
associationKey
    :   operatorExpression
    |   keyword
    ;

regexpLiteralContent
    :   REGULAR_EXPRESSION_BODY
    |   REGULAR_EXPRESSION_INTERPOLATION_BEGIN compoundStatement REGULAR_EXPRESSION_INTERPOLATION_END
    ;

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

symbol
    :   SYMBOL_LITERAL
    |   COLON singleQuotedString
    |   COLON doubleQuotedString
    ;


// --------------------------------------------------------
// Commons
// --------------------------------------------------------

assignmentOperator
    :   EQ
    |   ASSIGNMENT_OPERATOR
    ;

statementModifier
    :   IF
    |   UNLESS
    |   WHILE
    |   UNTIL
    |   RESCUE
    ;

variable
    :   CONSTANT_IDENTIFIER
    |   GLOBAL_VARIABLE_IDENTIFIER
    |   CLASS_VARIABLE_IDENTIFIER
    |   INSTANCE_VARIABLE_IDENTIFIER
    |   LOCAL_VARIABLE_IDENTIFIER
    ;

pseudoVariable
    :   NIL
    |   TRUE
    |   FALSE
    |   SELF
    |   LINE__
    |   FILE__
    |   ENCODING__
    ;

unsignedNumericLiteral
    :   DECIMAL_INTEGER_LITERAL
    |   BINARY_INTEGER_LITERAL
    |   OCTAL_INTEGER_LITERAL
    |   HEXADECIMAL_INTEGER_LITERAL
    |   FLOAT_LITERAL_WITHOUT_EXPONENT
    |   FLOAT_LITERAL_WITH_EXPONENT
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