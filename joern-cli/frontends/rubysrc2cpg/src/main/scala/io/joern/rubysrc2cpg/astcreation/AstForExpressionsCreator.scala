package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.ParserAst
import io.joern.rubysrc2cpg.parser.ParserAst.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.{RubyOperators, getBuiltInType}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForExpression(ctx: ParserRuleContext): Ast = {
    astForExpression(ParserAst(ctx))
  }

  protected def astForExpression(node: ParserNode): Ast = node match
    case node: StaticLiteralExpression  => astForStaticLiteral(node)
    case node: DynamicLiteralExpression => astForDynamicLiteral(node)
    case node: RangeExpression          => astForRangeExpression(node)
    case node: ArrayLiteral             => astForArrayLiteral(node)
    case node: HashLiteral              => astForHashLiteral(node)
    case node: Association              => astForAssociation(node)
    case node: UnaryExpression          => astForUnary(node)
    case node: BinaryExpression         => astForBinary(node)
    case node: ConditionalExpression    => astForConditionalExpression(node)
    case node: IndexAccess              => astForIndexAccess(node)
    case node: MemberAccess             => astForMemberAccess(node)
    case node: MemberCall               => astForMemberCall(node)
    case node: SimpleIdentifier         => astForSimpleIdentifier(node)
    case node: SimpleCall               => astForSimpleCall(node)
    case node: SingleAssignment         => astForSingleAssignment(node).last // TODO: handle any LOCAL nodes creation
    case _ =>
      logger.warn(
        s"Could not represent expression: ${node.text} (${node.getClass.getSimpleName}) (${filename}), skipping"
      )
      astForUnknown(node)

  // Member accesses are always CALLs, i.e. `x.y` is the call of `y` of `x` without any arguments.
  protected def astForMemberAccess(node: MemberAccess): Ast = {
    astForMemberCall(MemberCall(node.ctx, node.target, node.op, node.methodName, List()))
  }

  protected def astForMemberCall(node: MemberCall): Ast = {
    val methodFullName  = node.methodName // TODO
    val fieldAccessAst  = astForFieldAccess(MemberAccess(node.ctx, node.target, node.op, node.methodName))
    val arguments       = node.arguments.map(astForExpression)
    val fieldAccessCall = callNode(node, node.text, node.methodName, methodFullName, DispatchTypes.DYNAMIC_DISPATCH)
    callAst(fieldAccessCall, arguments, receiver = Some(fieldAccessAst))
  }

  protected def astForFieldAccess(node: MemberAccess): Ast = {
    val fieldIdentifier = Ast(fieldIdentifierNode(node, node.methodName, node.methodName))
    val target          = astForExpression(node.target)
    val dispatchType = node.op match
      case "::" => DispatchTypes.STATIC_DISPATCH
      case _    => DispatchTypes.DYNAMIC_DISPATCH
    val code        = s"${node.target.getText}${node.op}${node.methodName}"
    val fieldAccess = callNode(node, code, Operators.fieldAccess, Operators.fieldAccess, dispatchType)
    callAst(fieldAccess, Seq(target, fieldIdentifier))
  }

  protected def astForArrayLiteral(node: ArrayLiteral): Ast = {
    if (node.isDynamic) {
      logger.warn(s"Interpolated array literals are not supported yet: ${node.text}, skipping")
      astForUnknown(node)
    } else {
      val argumentsType = if (node.isStringArray) {
        getBuiltInType(Defines.String)
      } else {
        getBuiltInType(Defines.Symbol)
      }
      val argumentLiterals = node.elems.map(StaticLiteralExpression(_, argumentsType))
      val arguments        = argumentLiterals.map(astForExpression)
      val call_ =
        callNode(node, node.text, Operators.arrayInitializer, Operators.arrayInitializer, DispatchTypes.STATIC_DISPATCH)
      callAst(call_, arguments)
    }
  }

  protected def astForHashLiteral(node: HashLiteral): Ast = {
    val arguments = node.elems.flatMap(ctx =>
      ParserAst(ctx) match
        case assoc: Association => astForAssociation(assoc) :: Nil
        case _ =>
          logger.warn(s"Could not represent element: ${ctx.getText}, skipping")
          Seq()
    )
    val call_ = callNode(
      node,
      node.text,
      RubyOperators.hashInitializer,
      RubyOperators.hashInitializer,
      DispatchTypes.STATIC_DISPATCH
    )
    callAst(call_, arguments)
  }

  protected def astForAssociation(node: Association): Ast = {
    val key   = astForExpression(node.key)
    val value = astForExpression(node.value)
    val call_ =
      callNode(node, node.text, RubyOperators.association, RubyOperators.association, DispatchTypes.STATIC_DISPATCH)
    callAst(call_, Seq(key, value))
  }

  protected def astForRangeExpression(node: RangeExpression): Ast = {
    val lb    = astForExpression(node.lowerBound)
    val ub    = astForExpression(node.upperBound)
    val call_ = callNode(node, node.text, Operators.range, Operators.range, DispatchTypes.STATIC_DISPATCH)
    callAst(call_, Seq(lb, ub))
  }

  protected def astForDynamicLiteral(node: DynamicLiteralExpression): Ast = {
    val fmtValueAsts = node.expressions.map { exprCtx =>
      val exprNode = ParserAst(exprCtx).asInstanceOf[StatementList]
      if (exprNode.statements.size > 1) {
        logger.warn(
          s"Interpolations containing multiple statements are not supported yet: ${exprNode.ctx.getText}, skipping"
        )
        astForUnknown(exprNode)
      } else {
        val exprAst = astForExpression(exprNode.statements.head)
        val call = callNode(
          exprNode,
          exprNode.text,
          Operators.formattedValue,
          Operators.formattedValue,
          DispatchTypes.STATIC_DISPATCH,
          None,
          Some(node.typeFullName)
        )
        callAst(call, Seq(exprAst))
      }
    }
    val fmtStringAst = callAst(
      callNode(
        node,
        node.text,
        Operators.formatString,
        Operators.formatString,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(node.typeFullName)
      ),
      fmtValueAsts
    )
    fmtStringAst
  }

  protected def astForSimpleCall(node: SimpleCall): Ast = {
    ParserAst(node.target) match
      case targetNode: SimpleIdentifier => astForMethodCallWithoutBlock(node, targetNode)
      case targetNode: MemberAccess     => astForMemberCallWithoutBlock(node, targetNode)
      case _ =>
        logger.warn(s"Unrecognized target of call: ${node.target.getText}, skipping")
        astForUnknown(node)
  }

  protected def astForIndexAccess(node: IndexAccess): Ast = {
    val indexesAsts = node.indexes.map(astForExpression)
    val targetAst   = astForExpression(node.target)
    val indexCall =
      callNode(node, node.text, Operators.indexAccess, Operators.indexAccess, DispatchTypes.STATIC_DISPATCH)
    callAst(indexCall, indexesAsts, receiver = Some(targetAst))
  }

  // for membered, non-block calls, e.g. self.foo 1, x::y&.z.w(1,2)
  protected def astForMemberCallWithoutBlock(node: SimpleCall, memberAccess: MemberAccess): Ast = {
    val receiverAst    = astForFieldAccess(memberAccess)
    val methodName     = memberAccess.methodName
    val methodFullName = methodName // TODO: ?
    val argumentsAst   = node.arguments.map(astForExpression)
    val call           = callNode(node, node.text, methodName, methodFullName, DispatchTypes.DYNAMIC_DISPATCH)
    callAst(call, argumentsAst, None, Some(receiverAst))
  }

  // for non-membered, non-block calls, e.g. foo 1, foo(1,2)
  private def astForMethodCallWithoutBlock(node: SimpleCall, methodIdentifier: SimpleIdentifier): Ast = {
    val methodName     = methodIdentifier.ctx.getText
    val methodFullName = methodName // TODO: ?
    val argumentsAst   = node.arguments.map(astForExpression)
    val call           = callNode(node, node.text, methodName, methodFullName, DispatchTypes.DYNAMIC_DISPATCH)
    callAst(call, argumentsAst, None, None)
  }

  // TODO: Check for builtin variable names
  protected def astForSimpleIdentifier(node: SimpleIdentifier): Ast = {
    val identifier = node.text
    scope.lookupVariable(identifier) match
      case None =>
        val call = SimpleCall(node.ctx, node.ctx, List())
        astForSimpleCall(call)
      case Some(_) =>
        val idNode = identifierNode(node, node.text, node.text, node.typeFullName.getOrElse(""))
        Ast(idNode)
  }

  protected def astForConditionalExpression(node: ConditionalExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val thenAst      = astForExpression(node.trueBranch)
    val elseAst      = astForExpression(node.falseBranch)
    val call = callNode(node, node.text, Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH)
    callAst(call, Seq(conditionAst, thenAst, elseAst))
  }

  protected def astForBinary(node: BinaryExpression): Ast = {
    getBinaryOperatorName(node.op) match
      case None =>
        logger.warn(s"Unrecognized binary operator: ${node.text}, skipping")
        astForUnknown(node)
      case Some(op) =>
        val lhs  = astForExpression(node.lhs)
        val rhs  = astForExpression(node.rhs)
        val call = callNode(node, node.text, op, op, DispatchTypes.STATIC_DISPATCH)
        callAst(call, Seq(lhs, rhs))
  }

  protected def astForUnary(node: UnaryExpression): Ast = {
    getUnaryOperatorName(node.op) match
      case None =>
        logger.warn(s"Unrecognized unary operator: ${node.text}, skipping")
        astForUnknown(node)
      case Some(op) =>
        val expr = astForExpression(node.expr)
        val call = callNode(node, node.text, op, op, DispatchTypes.STATIC_DISPATCH)
        callAst(call, Seq(expr))
  }

  protected def astForStaticLiteral(node: StaticLiteralExpression): Ast = {
    Ast(literalNode(node, node.text, node.typeFullName))
  }

  protected def astForUnknown(node: ParserNode): Ast = {
    Ast(unknownNode(node, node.text))
  }

  private def getBinaryOperatorName(op: String): Option[String] = {
    Map(
      "+"   -> Operators.addition,
      "-"   -> Operators.subtraction,
      "*"   -> Operators.multiplication,
      "/"   -> Operators.division,
      "%"   -> Operators.modulo,
      "**"  -> Operators.exponentiation,
      "=="  -> Operators.equals,
      "!="  -> Operators.notEquals,
      "<"   -> Operators.lessThan,
      "<="  -> Operators.lessEqualsThan,
      ">"   -> Operators.greaterThan,
      ">="  -> Operators.greaterEqualsThan,
      "<=>" -> Operators.compare,
      "&&"  -> Operators.logicalAnd,
      "and" -> Operators.logicalAnd,
      "or"  -> Operators.logicalOr,
      "||"  -> Operators.logicalOr,
      "&"   -> Operators.and,
      "|"   -> Operators.or,
      "^"   -> Operators.xor,
      "<<"  -> Operators.shiftLeft,
      ">>"  -> Operators.logicalShiftRight
    ).get(op)
  }

  private def getUnaryOperatorName(op: String): Option[String] = {
    Map(
      "!"   -> Operators.logicalNot,
      "not" -> Operators.logicalNot,
      "~"   -> Operators.not,
      "+"   -> Operators.plus,
      "-"   -> Operators.minus
    ).get(op)
  }

}
