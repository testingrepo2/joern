package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.ParserAst.*
import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import org.antlr.v4.runtime.ParserRuleContext
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

object ParserAstCreator {

  /** Translates an ANTLR concrete syntax tree node (`ParserRuleContext`) into a custom `ParserAst.ParserNode` node. */
  @tailrec
  def translate(ctx: ParserRuleContext): ParserNode = ctx match
    //
    // Follow-through contexts
    //
    case ctx: ProgramContext                                                 => translate(ctx.compoundStatement())
    case ctx: CompoundStatementContext if Option(ctx.statements()).isDefined => translate(ctx.statements())
    case ctx: ExpressionOrCommandStatementContext                            => translate(ctx.expressionOrCommand())
    case ctx: OperatorExpressionOrCommandContext                             => translate(ctx.operatorExpression())
    case ctx: PrimaryOperatorExpressionContext                               => translate(ctx.primary())
    case ctx: PrimaryValuePrimaryContext                                     => translate(ctx.primaryValue())
    case ctx: MethodCallOrVariableReferenceContext                           => translate(ctx.variableReference())
    case ctx: VariableVariableReferenceContext                               => translate(ctx.variable())
    case ctx: PseudoVariableVariableReferenceContext                         => translate(ctx.pseudoVariable())
    case ctx: SymbolExpressionContext                                        => translate(ctx.symbol())
    case ctx: NumericLiteralContext if Option(ctx.sign).isEmpty              => translate(ctx.unsignedNumericLiteral())
    case ctx: DoClauseContext                                                => translate(ctx.compoundStatement())
    case ctx: PrimaryValueCommandOrPrimaryValueContext                       => translate(ctx.primaryValue())
    case ctx: MandatoryMandatoryOrOptionalParameterContext                   => translate(ctx.mandatoryParameter())
    case ctx: OptionalMandatoryOrOptionalParameterContext                    => translate(ctx.optionalParameter())
    case ctx: VariableReferenceSingletonObjectContext                        => translate(ctx.variableReference())
    case ctx: AssociationKeyContext if Option(ctx.operatorExpression()).isDefined => translate(ctx.operatorExpression())
    case ctx: CommandExpressionOrCommandContext if Option(ctx.EMARK()).isEmpty =>
      translate(ctx.methodInvocationWithoutParentheses())
    case ctx: CommandMethodInvocationWithoutParenthesesContext => translate(ctx.command())
    case ctx: ThenClauseContext                                => translate(ctx.compoundStatement())
    case ctx: BodyStatementContext
        if ctx.rescueClause().isEmpty && Option(ctx.elseClause()).isEmpty && Option(ctx.ensureClause()).isEmpty =>
      translate(ctx.compoundStatement())
    case ctx: DoBlockBlockContext => translate(ctx.doBlock())

    //
    // Statements
    //
    case ctx: CompoundStatementContext if Option(ctx.statements()).isEmpty => StatementList(ctx, List())
    case ctx: StatementsContext => StatementList(ctx, ctx.statement().asScala.toList)

    //
    // Declarations
    //
    case ctx: ModuleDefinitionContext =>
      ModuleDeclaration(ctx, ctx.classPath(), ctx.bodyStatement())

    case ctx: ClassDefinitionContext =>
      ClassDeclaration(ctx, ctx.classPath(), Option(ctx.commandOrPrimaryValue()), ctx.bodyStatement())
    case ctx: MethodDefinitionContext =>
      MethodDeclaration(
        ctx,
        ctx.definedMethodName().getText,
        Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters),
        ctx.bodyStatement()
      )
    case ctx: EndlessMethodDefinitionContext =>
      MethodDeclaration(
        ctx,
        ctx.definedMethodName().getText,
        Option(ctx.parameterList()).fold(List())(_.parameters),
        ctx.commandOrPrimaryValue()
      )
    case ctx: SingletonMethodDefinitionContext =>
      SingletonMethodDeclaration(
        ctx,
        ctx.singletonObject(),
        ctx.definedMethodName().getText,
        Option(ctx.methodParameterPart().parameterList()).fold(List())(_.parameters),
        ctx.bodyStatement()
      )
    case ctx: SimpleCommandContext if ctx.methodIdentifier().isAttrDeclaration =>
      FieldDeclarations(ctx, ctx.commandArgument().arguments)

    //
    // Parameters
    //
    case ctx: ProcParameterContext      => ProcParameter(ctx, ctx.procParameterName())
    case ctx: HashParameterContext      => HashParameter(ctx, Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText))
    case ctx: ArrayParameterContext     => ArrayParameter(ctx, Option(ctx.LOCAL_VARIABLE_IDENTIFIER()).map(_.getText))
    case ctx: OptionalParameterContext  => OptionalParameter(ctx, ctx.optionalParameterName(), ctx.operatorExpression())
    case ctx: MandatoryParameterContext => MandatoryParameter(ctx)
    case ctx: VariableLeftHandSideContext if Option(ctx.primary()).isEmpty => MandatoryParameter(ctx)

    //
    // Control structures
    //
    case ctx: WhileExpressionContext => WhileExpression(ctx, ctx.commandOrPrimaryValue(), ctx.doClause())
    case ctx: UntilExpressionContext => UntilExpression(ctx, ctx.commandOrPrimaryValue(), ctx.doClause())
    case ctx: IfExpressionContext =>
      IfExpression(
        ctx,
        ctx.commandOrPrimaryValue(),
        ctx.thenClause(),
        ctx.elsifClause().asScala.toList,
        Option(ctx.elseClause())
      )
    case ctx: ElsifClauseContext => ElsIfClause(ctx, ctx.expressionOrCommand(), ctx.thenClause())
    case ctx: ElseClauseContext  => ElseClause(ctx, ctx.compoundStatement())
    case ctx: UnlessExpressionContext =>
      UnlessExpression(ctx, ctx.commandOrPrimaryValue(), ctx.thenClause(), Option(ctx.elseClause()))
    case ctx: ModifierStatementContext if Option(ctx.statementModifier().UNLESS()).isDefined =>
      UnlessExpression(ctx, ctx.expressionOrCommand(), ctx.statement(), None)
    case ctx: ModifierStatementContext if Option(ctx.statementModifier().IF()).isDefined =>
      IfExpression(ctx, ctx.expressionOrCommand(), ctx.statement(), List(), None)
    case ctx: ReturnMethodInvocationWithoutParenthesesContext =>
      ReturnExpression(ctx, ctx.primaryValueList().primaryValue().asScala.toList)
    case ctx: TernaryOperatorExpressionContext =>
      ConditionalExpression(ctx, ctx.operatorExpression(0), ctx.operatorExpression(1), ctx.operatorExpression(2))

    //
    // Unary expressions
    //
    case ctx: NumericLiteralContext if Option(ctx.sign).isDefined =>
      UnaryExpression(ctx, ctx.sign.getText, ctx.unsignedNumericLiteral())
    case ctx: UnaryExpressionContext        => UnaryExpression(ctx, ctx.unaryOperator().getText, ctx.primaryValue())
    case ctx: UnaryMinusExpressionContext   => UnaryExpression(ctx, ctx.MINUS().getText, ctx.primaryValue())
    case ctx: NotExpressionOrCommandContext => UnaryExpression(ctx, ctx.NOT().getText, ctx.expressionOrCommand())
    case ctx: CommandExpressionOrCommandContext if Option(ctx.EMARK()).isDefined =>
      UnaryExpression(ctx, "!", ctx.methodInvocationWithoutParentheses())

    //
    // Binary expressions
    //
    case ctx: PowerExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.powerOperator.getText, ctx.primaryValue(1))
    case ctx: AdditiveExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.additiveOperator().getText, ctx.primaryValue(1))
    case ctx: MultiplicativeExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.multiplicativeOperator().getText, ctx.primaryValue(1))
    case ctx: LogicalAndExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.andOperator.getText, ctx.primaryValue(1))
    case ctx: LogicalOrExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.orOperator.getText, ctx.primaryValue(1))
    case ctx: KeywordAndOrExpressionOrCommandContext =>
      BinaryExpression(ctx, ctx.lhs, ctx.binOp.getText, ctx.rhs)
    case ctx: ShiftExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.bitwiseShiftOperator().getText, ctx.primaryValue(1))
    case ctx: BitwiseAndExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.bitwiseAndOperator.getText, ctx.primaryValue(1))
    case ctx: BitwiseOrExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.bitwiseOrOperator().getText, ctx.primaryValue(1))
    case ctx: RelationalExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.relationalOperator().getText, ctx.primaryValue(1))
    case ctx: EqualityExpressionContext =>
      BinaryExpression(ctx, ctx.primaryValue(0), ctx.equalityOperator().getText, ctx.primaryValue(1))

    //
    // Literals
    //
    case ctx: DecimalUnsignedLiteralContext     => StaticLiteralExpression(ctx, getBuiltInType(Defines.Integer))
    case ctx: BinaryUnsignedLiteralContext      => StaticLiteralExpression(ctx, getBuiltInType(Defines.Integer))
    case ctx: OctalUnsignedLiteralContext       => StaticLiteralExpression(ctx, getBuiltInType(Defines.Integer))
    case ctx: HexadecimalUnsignedLiteralContext => StaticLiteralExpression(ctx, getBuiltInType(Defines.Integer))
    case ctx: FloatWithoutExponentUnsignedLiteralContext => StaticLiteralExpression(ctx, getBuiltInType(Defines.Float))
    case ctx: FloatWithExponentUnsignedLiteralContext    => StaticLiteralExpression(ctx, getBuiltInType(Defines.Float))
    case ctx: PureSymbolLiteralContext                   => StaticLiteralExpression(ctx, getBuiltInType(Defines.Symbol))
    case ctx: SingleQuotedSymbolLiteralContext           => StaticLiteralExpression(ctx, getBuiltInType(Defines.Symbol))
    case ctx: NilPseudoVariableContext   => StaticLiteralExpression(ctx, getBuiltInType(Defines.NilClass))
    case ctx: TruePseudoVariableContext  => StaticLiteralExpression(ctx, getBuiltInType(Defines.TrueClass))
    case ctx: FalsePseudoVariableContext => StaticLiteralExpression(ctx, getBuiltInType(Defines.FalseClass))
    case ctx: SingleQuotedStringExpressionContext if !ctx.isInterpolated =>
      StaticLiteralExpression(ctx, getBuiltInType(Defines.String))
    case ctx: QuotedNonExpandedStringLiteralContext =>
      StaticLiteralExpression(ctx, getBuiltInType(Defines.String))
    case ctx: DoubleQuotedStringExpressionContext if !ctx.isInterpolated =>
      StaticLiteralExpression(ctx, getBuiltInType(Defines.String))
    case ctx: RegularExpressionLiteralContext if ctx.isStatic =>
      StaticLiteralExpression(ctx, getBuiltInType(Defines.Regexp))

    //
    // Dynamic literals
    //
    case ctx: SingleQuotedStringExpressionContext if ctx.isInterpolated =>
      DynamicLiteralExpression(ctx, getBuiltInType(Defines.String), ctx.interpolations)
    case ctx: DoubleQuotedStringExpressionContext if ctx.isInterpolated =>
      DynamicLiteralExpression(ctx, getBuiltInType(Defines.String), ctx.interpolations)

    //
    // Ranges
    //
    case ctx: RangeExpressionContext =>
      RangeExpression(ctx, ctx.primaryValue(0), ctx.primaryValue(1))

    //
    // Array literals
    //
    case ctx: BracketedArrayLiteralContext =>
      ArrayLiteral(ctx, Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(Seq()))
    case ctx: QuotedNonExpandedStringArrayLiteralContext =>
      ArrayLiteral(ctx, Option(ctx.quotedNonExpandedArrayElementList()).map(_.elements).getOrElse(Seq()))
    case ctx: QuotedNonExpandedSymbolArrayLiteralContext =>
      ArrayLiteral(ctx, Option(ctx.quotedNonExpandedArrayElementList()).map(_.elements).getOrElse(Seq()))

    //
    // Hash literals
    //
    case ctx: HashLiteralContext =>
      HashLiteral(ctx, Option(ctx.associationList()).map(_.associations).getOrElse(List()))
    case ctx: AssociationContext =>
      Association(ctx, ctx.associationKey(), ctx.operatorExpression())

    //
    // Built-in global variables
    //
    case ctx: LinePseudoVariableContext     => SimpleIdentifier(ctx, Some(getBuiltInType(Defines.Integer)))
    case ctx: FilePseudoVariableContext     => SimpleIdentifier(ctx, Some(getBuiltInType(Defines.String)))
    case ctx: EncodingPseudoVariableContext => SimpleIdentifier(ctx, Some(getBuiltInType(Defines.Encoding)))
    case ctx: SelfPseudoVariableContext     => SelfIdentifier(ctx)

    //
    // Identifiers
    //
    case ctx: ConstantIdentifierVariableContext => SimpleIdentifier(ctx, None)
    case ctx: GlobalIdentifierVariableContext   => SimpleIdentifier(ctx, None)
    case ctx: ClassIdentifierVariableContext    => SimpleIdentifier(ctx, None)
    case ctx: InstanceIdentifierVariableContext => SimpleIdentifier(ctx, None)
    case ctx: LocalIdentifierVariableContext    => SimpleIdentifier(ctx, None)
    case ctx: ClassNameContext                  => SimpleIdentifier(ctx, None)
    case ctx: MethodIdentifierContext           => SimpleIdentifier(ctx, None)
    case ctx: IsDefinedKeywordContext           => SimpleIdentifier(ctx, None)

    //
    // Assignments
    //
    case ctx: LocalVariableAssignmentExpressionContext =>
      SingleAssignment(ctx, ctx.lhs, ctx.assignmentOperator().getText, ctx.rhs)
    case ctx: AttributeAssignmentExpressionContext =>
      AttributeAssignment(ctx, ctx.primaryValue(), ctx.op.getText, ctx.methodName().getText, ctx.operatorExpression())

    //
    // Invocations
    //
    case ctx: SimpleCommandContext =>
      SimpleCall(ctx, ctx.methodIdentifier(), ctx.commandArgument().arguments)
    case ctx: IsDefinedExpressionContext =>
      SimpleCall(ctx, ctx.isDefinedKeyword, List(ctx.expressionOrCommand()))
    case ctx: IsDefinedCommandContext =>
      SimpleCall(ctx, ctx.isDefinedKeyword, List(ctx.primaryValue()))
    case ctx: MethodCallExpressionContext =>
      SimpleCall(ctx, ctx.methodOnlyIdentifier(), List())
    case ctx: MethodCallWithBlockExpressionContext =>
      CallWithBlock(ctx, ctx.methodIdentifier(), List(), ctx.block())
    case ctx: MethodCallWithParenthesesExpressionContext =>
      if (Option(ctx.block()).isDefined) {
        CallWithBlock(ctx, ctx.methodIdentifier(), ctx.argumentWithParentheses().arguments, ctx.block())
      } else {
        SimpleCall(ctx, ctx.methodIdentifier(), ctx.argumentWithParentheses().arguments)
      }
    case ctx: MemberAccessExpressionContext
        if Option(ctx.argumentWithParentheses()).isDefined && Option(ctx.block()).isEmpty =>
      MemberCall(
        ctx,
        ctx.primaryValue(),
        ctx.op.getText,
        ctx.methodName().getText,
        ctx.argumentWithParentheses().arguments
      )
    case ctx: MemberAccessExpressionContext if Option(ctx.block()).isDefined =>
      MemberCallWithBlock(
        ctx,
        ctx.primaryValue(),
        ctx.op.getText,
        ctx.methodName().getText,
        Option(ctx.argumentWithParentheses()).map(_.arguments).getOrElse(List()),
        ctx.block()
      )

    //
    // Member accesses
    //
    case ctx: MemberAccessExpressionContext
        if Option(ctx.argumentWithParentheses).isEmpty && Option(ctx.block()).isEmpty =>
      MemberAccess(ctx, ctx.primaryValue(), ctx.op.getText, ctx.methodName().getText)
    case ctx: IndexingAccessExpressionContext =>
      IndexAccess(ctx, ctx.primaryValue(), Option(ctx.indexingArgumentList()).map(_.arguments).getOrElse(List()))

    //
    // Blocks
    //
    case ctx: CurlyBracesBlockContext =>
      Block(ctx, Option(ctx.blockParameter()).fold(List())(_.parameters), ctx.compoundStatement())
    case ctx: DoBlockContext =>
      Block(ctx, Option(ctx.blockParameter()).fold(List())(_.parameters), ctx.bodyStatement())

    //
    // Fallback
    //
    case _ => Unknown(ctx)
}

sealed implicit class QuotedNonExpandedArrayElementListContextHelper(ctx: QuotedNonExpandedArrayElementListContext) {
  def elements: List[ParserRuleContext] = ctx.quotedNonExpandedArrayElementContent().asScala.toList
}

sealed implicit class RegularExpressionLiteralContextHelper(ctx: RegularExpressionLiteralContext) {
  def isStatic: Boolean  = !isDynamic
  def isDynamic: Boolean = ctx.regexpLiteralContent.asScala.exists(c => Option(c.compoundStatement()).isDefined)
}

sealed implicit class AssociationListContextHelper(ctx: AssociationListContext) {
  def associations: List[ParserRuleContext] = ctx.association().asScala.toList
}

sealed implicit class MethodIdentifierContextHelper(ctx: MethodIdentifierContext) {
  def isAttrDeclaration: Boolean = Set("attr_reader", "attr_writer", "attr_accessor").contains(ctx.getText)
}

sealed implicit class ArgumentWithParenthesesContextHelper(ctx: ArgumentWithParenthesesContext) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def arguments: List[ParserRuleContext] = ctx match
    case _: EmptyArgumentWithParenthesesContext          => List()
    case ctx: ArgumentListArgumentWithParenthesesContext => ctx.argumentList().elements
    case ctx =>
      logger.warn(s"Could not extract arguments: ${ctx.getText}, skipping")
      List()
}

sealed implicit class ArgumentListContextHelper(ctx: ArgumentListContext) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def elements: List[ParserRuleContext] = ctx match
    case ctx: OperatorsArgumentListContext =>
      val operatorExpressions = ctx.operatorExpressionList().operatorExpression().asScala.toList
      val associations        = Option(ctx.associationList()).fold(List())(_.association().asScala)
      val splatting           = Option(ctx.splattingArgument()).toList
      val block               = Option(ctx.blockArgument()).toList
      operatorExpressions ++ associations ++ splatting ++ block
    case ctx =>
      logger.warn(s"Could not extract arguments: ${ctx.getText}, skipping")
      List()
}

/** Helper to extract the parameters of a `CurlyBracesBlockContext` */
sealed implicit class CurlyBracesBlockContextHelper(ctx: CurlyBracesBlockContext) {
  def parameters: List[ParserRuleContext] = Option(ctx.blockParameter()).map(_.parameters).getOrElse(List())
}

/** Helper to extract the parameters of a `BlockParameterContext` */
sealed implicit class BlockParameterContextHelper(ctx: BlockParameterContext) {
  def parameters: List[ParserRuleContext] = Option(ctx.blockParameterList()).map(_.parameters).getOrElse(List())
}

/** Helper to extract the parameters of a `BlockParameterListContext` */
sealed implicit class BlockParameterListContextHelper(ctx: BlockParameterListContext) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def parameters: List[ParserRuleContext] = ctx match
    case ctx: SingleElementBlockParameterListContext => ctx.leftHandSide() :: Nil
    case ctx =>
      logger.warn(s"N-ary parameter blocks are not supported yet: ${ctx.getText}, skipping")
      List()
}

/** Helper to extract the arguments of a `CommandArgumentContext` */
sealed implicit class CommandArgumentContextHelper(ctx: CommandArgumentContext) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def arguments: List[ParserRuleContext] = ctx match
    case ctx: CommandCommandArgumentListContext         => ctx.command() :: Nil
    case ctx: CommandArgumentCommandArgumentListContext => ctx.commandArgumentList().elements
    case _ =>
      logger.warn(s"Could not extract arguments: ${ctx.getText}, skipping")
      List()
}

/** Helper to extract the elements of a `CommandArgumentListContext` */
sealed implicit class CommandArgumentListContextHelper(ctx: CommandArgumentListContext) {
  def elements: List[ParserRuleContext] = {
    val primaryValues = Option(ctx.primaryValueList()).map(_.primaryValue().asScala.toList).getOrElse(List())
    val associations  = Option(ctx.associationList()).map(_.association().asScala.toList).getOrElse(List())
    primaryValues ++ associations
  }
}

/** Helper to extract the parameters of a `ParameterListContext` */
sealed implicit class ParameterListContextHelper(ctx: ParameterListContext) {
  def parameters: List[ParserRuleContext] = {
    val mandatoryOrOptionals = Option(ctx.mandatoryOrOptionalParameterList()).map(_.parameters).getOrElse(List())
    val arrayParameter       = Option(ctx.arrayParameter()).toList
    val hashParameter        = Option(ctx.hashParameter()).toList
    val procParameter        = Option(ctx.procParameter()).toList
    mandatoryOrOptionals ++ arrayParameter ++ hashParameter ++ procParameter
  }
}

/** Helper to extract the arguments of a `IndexingArgumentListContext` */
sealed implicit class IndexingArgumentListContextHelper(ctx: IndexingArgumentListContext) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def arguments: List[ParserRuleContext] = ctx match
    case ctx: CommandIndexingArgumentListContext => List(ctx.command)
    case ctx: OperatorExpressionListIndexingArgumentListContext =>
      ctx.operatorExpressionList().operatorExpression().asScala.toList
    case _ =>
      logger.warn(s"Could not extract arguments: ${ctx.getText}, skipping")
      List()
}

sealed implicit class MandatoryOrOptionalParameterListContextHelper(ctx: MandatoryOrOptionalParameterListContext) {
  def parameters: List[ParserRuleContext] = ctx.mandatoryOrOptionalParameter().asScala.toList
}

sealed implicit class MethodParameterPartContextHelper(ctx: MethodParameterPartContext) {
  def parameters: List[ParserRuleContext] = Option(ctx.parameterList()).map(_.parameters).getOrElse(List())
}

// TODO: The following are helpers to extract:
//  (1) the concatenations in a string expression, e.g. 'x' 'y', that ought to become just a single literal
//  (2) the interpolated expressions, e.g. "#{x}" "#{y}", that ought to extract the identifiers x and y, to be used in
//     a <operator>.formatString and <operator>.formattedValue sequence of calls.
//  This needs to be rewritten as there are surely needless distinctions in place.

sealed implicit class SingleQuotedStringExpressionContextHelper(ctx: SingleQuotedStringExpressionContext) {
  def concatenations: List[SingleOrDoubleQuotedStringContext] = ctx.singleOrDoubleQuotedString().asScala.toList
  def isInterpolated: Boolean                                 = concatenations.exists(_.isInterpolated)
  def interpolations: List[ParserRuleContext] =
    concatenations.filter(_.isInterpolated).flatMap(_.doubleQuotedString().interpolations)
}

sealed implicit class DoubleQuotedStringExpressionContextHelper(ctx: DoubleQuotedStringExpressionContext) {
  def interpolations: List[ParserRuleContext] = ctx.doubleQuotedString().interpolations ++ ctx
    .singleOrDoubleQuotedString()
    .asScala
    .filter(_.isInterpolated)
    .flatMap(_.doubleQuotedString().interpolations)
    .toList
  def concatenations: List[SingleOrDoubleQuotedStringContext] = ctx.singleOrDoubleQuotedString().asScala.toList
  def isInterpolated: Boolean = ctx.doubleQuotedString().isInterpolated || concatenations.exists(_.isInterpolated)
}

sealed implicit class DoubleQuotedStringContextHelper(ctx: DoubleQuotedStringContext) {
  def interpolations: List[ParserRuleContext] = ctx
    .doubleQuotedStringContent()
    .asScala
    .filter(ctx => Option(ctx.compoundStatement()).isDefined)
    .map(ctx => ctx.compoundStatement())
    .toList
  def isInterpolated: Boolean = interpolations.nonEmpty
}

sealed implicit class SingleOrDoubleQuotedStringContextHelper(ctx: SingleOrDoubleQuotedStringContext) {
  def isInterpolated: Boolean = Option(ctx.doubleQuotedString()).exists(_.isInterpolated)
}
