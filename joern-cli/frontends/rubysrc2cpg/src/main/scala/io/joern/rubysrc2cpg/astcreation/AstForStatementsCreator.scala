package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.ParserAst
import io.joern.rubysrc2cpg.parser.ParserAst.*
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext

trait AstForStatementsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astsForStatement(ctx: ParserRuleContext): Seq[Ast] = {
    astsForStatement(ParserAst(ctx))
  }

  protected def astsForStatement(node: ParserNode): Seq[Ast] = node match
    case node: WhileExpression            => astForWhileStatement(node) :: Nil
    case node: UntilExpression            => astForUntilStatement(node) :: Nil
    case node: IfExpression               => astForIfStatement(node) :: Nil
    case node: UnlessExpression           => astForUnlessStatement(node) :: Nil
    case node: ReturnExpression           => astForReturnStatement(node) :: Nil
    case node: SingleAssignment           => astForSingleAssignment(node)
    case node: AttributeAssignment        => astForAttributeAssignment(node) :: Nil
    case node: ModuleDeclaration          => astForModuleDeclaration(node) :: Nil
    case node: ClassDeclaration           => astForClassDeclaration(node) :: Nil
    case node: FieldDeclarations          => astsForFieldDeclarations(node)
    case node: MethodDeclaration          => astForMethodDeclaration(node) :: Nil
    case node: SingletonMethodDeclaration => astForSingletonMethodDeclaration(node) :: Nil
    case node: StatementList              => astForStatementList(node) :: Nil
    case node: MemberCallWithBlock        => astForMemberCallWithBlock(node) :: Nil
    case node: CallWithBlock              => astForCallWithBlock(node) :: Nil
    case _                                => astForExpression(node) :: Nil

  // Lowered as a BLOCK node.
  // foo(<args>) do <params> <stmts> end is lowered as a BLOCK node shaped like so:
  //  {
  //    <params> = foo(<args>)
  //    <stmts>
  //  }
  // If <params> is empty, we simply exclude the initial assignment (but keep the invocation)
  // TODO: this representation is not final. A better representation is to follow Ruby's
  //  semantics and pass in the block as an argument (a closure) to `foo`, i.e. `foo(<args>, <block>)`.
  protected def astForCallWithBlock(node: CallWithBlock): Ast = {
    val rubyBlock   = ParserAst(node.block).asInstanceOf[Block]
    val blockParams = rubyBlock.parameters
    if (blockParams.nonEmpty) {
      logger.warn(s"Blocks with parameters are not supported yet: ${node.text}, skipping")
      astForUnknown(node)
    } else {
      val outerBlock = blockNode(node)
      val callAst    = astForSimpleCall(node.withoutBlock)
      methodAstParentStack.push(outerBlock)
      scope.pushNewScope(outerBlock)
      val stmtAsts = ParserAst(rubyBlock.body) match
        case stmtList: StatementList => stmtList.statements.flatMap(astsForStatement)
        case body =>
          logger.warn(s"Non-linear method bodies are not supported yet: ${body.text}, skipping")
          List()
      scope.popScope()
      methodAstParentStack.pop()
      blockAst(outerBlock, callAst :: stmtAsts)
    }
  }

  protected def astForMemberCallWithBlock(node: ParserAst.MemberCallWithBlock): Ast = {
    val rubyBlock   = ParserAst(node.block).asInstanceOf[Block]
    val blockParams = rubyBlock.parameters
    if (blockParams.nonEmpty) {
      logger.warn(s"Blocks with parameters are not supported yet: ${node.text}, skipping")
      astForUnknown(node)
    } else {
      val outerBlock = blockNode(node)
      val callAst    = astForMemberCall(node.withoutBlock)
      methodAstParentStack.push(outerBlock)
      scope.pushNewScope(outerBlock)
      val stmtAsts = ParserAst(rubyBlock.body) match
        case stmtList: StatementList => stmtList.statements.flatMap(astsForStatement)
        case body =>
          logger.warn(s"Non-linear method bodies are not supported yet: ${body.text}, skipping")
          List()
      scope.popScope()
      methodAstParentStack.pop()
      blockAst(outerBlock, callAst :: stmtAsts)
    }
  }

  protected def astForStatementList(node: StatementList): Ast = {
    val block = blockNode(node)
    scope.pushNewScope(block)
    val stmtAsts = node.statements.flatMap(astsForStatement)
    scope.popScope()
    blockAst(block, stmtAsts.toList)
  }

  protected def astForStatementListReturningLastExpression(node: StatementList): Ast = {
    val block = blockNode(node)
    scope.pushNewScope(block)

    val stmtAsts = if (node.statements.isEmpty) {
      Seq()
    } else if (node.statements.length == 1) {
      astsForImplicitReturnStatement(node.statements.head)
    } else {
      val headStmts = node.statements.take(node.statements.length - 1)
      val lastStmt  = node.statements.last
      headStmts.flatMap(astsForStatement) ++ astsForImplicitReturnStatement(lastStmt)
    }

    scope.popScope()
    blockAst(block, stmtAsts.toList)

  }

  protected def astsForImplicitReturnStatement(ctx: ParserRuleContext): Seq[Ast] = {
    ParserAst(ctx) match
      case _: StaticLiteralExpression => astForReturnStatement(ReturnExpression(ctx, List(ctx))) :: Nil
      case _: BinaryExpression        => astForReturnStatement(ReturnExpression(ctx, List(ctx))) :: Nil
      case _: UnaryExpression         => astForReturnStatement(ReturnExpression(ctx, List(ctx))) :: Nil
      case node: SingleAssignment =>
        astForSingleAssignment(node) ++ Seq(astForReturnStatement(ReturnExpression(ctx, List(node.lhs))))
      case node: AttributeAssignment =>
        Seq(
          astForAttributeAssignment(node),
          astForReturnFieldAccess(MemberAccess(node.ctx, node.target, node.op, node.methodName))
        )
      case node: MemberAccess    => astForReturnMemberCall(node) :: Nil
      case ret: ReturnExpression => astForReturnStatement(ret) :: Nil
      case node =>
        logger.warn(
          s"Implicit return here not supported yet: ${ctx.getText} (${node.getClass.getSimpleName}), skipping"
        )
        Seq()
  }

  protected def astForReturnFieldAccess(node: MemberAccess): Ast = {
    returnAst(returnNode(node, node.text), List(astForFieldAccess(node)))
  }

  protected def astForReturnMemberCall(node: MemberAccess): Ast = {
    returnAst(returnNode(node, node.text), List(astForMemberAccess(node)))
  }

  protected def astForReturnStatement(node: ReturnExpression): Ast = {
    val arguments   = node.expressions.map(astForExpression)
    val returnNode_ = returnNode(node, node.text)
    returnAst(returnNode_, arguments)
  }

  // unless T do B is lowered as if !T then B
  protected def astForUnlessStatement(node: UnlessExpression): Ast = {
    val notConditionAst = astForExpression(UnaryExpression(node.condition, "!", node.condition))
    val thenAst = ParserAst(node.trueBranch) match
      case trueBranch: StatementList => astForStatementList(trueBranch)
      case _                         => astForStatementList(StatementList(node.trueBranch, List(node.trueBranch)))
    val elseAst = node.falseBranch.map(ctx => astForElseClause(ParserAst(ctx).asInstanceOf[ElseClause])).toList
    val ifNode  = controlStructureNode(node, ControlStructureTypes.IF, node.text)
    controlStructureAst(ifNode, Some(notConditionAst), thenAst :: elseAst)
  }

  protected def astForIfStatement(node: IfExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val thenAst = ParserAst(node.thenClause) match
      case stmtList: StatementList => astForStatementList(stmtList)
      case _                       => astForStatementList(StatementList(node.thenClause, List(node.thenClause)))
    val elseAst = node.elsifClauses match
      case Nil => node.elseClause.map(ctx => astForElseClause(ParserAst(ctx).asInstanceOf[ElseClause])).toList
      case elsIfCtx :: rest =>
        val elsIf         = ParserAst(elsIfCtx).asInstanceOf[ElsIfClause]
        val newIf         = IfExpression(elsIf.ctx, elsIf.condition, elsIf.thenClause, rest, node.elseClause)
        val wrappingBlock = blockNode(elsIf)
        val wrappedAst    = Ast(wrappingBlock).withChild(astForIfStatement(newIf))
        wrappedAst :: Nil

    val ifNode = controlStructureNode(node, ControlStructureTypes.IF, node.text)
    controlStructureAst(ifNode, Some(conditionAst), thenAst :: elseAst)
  }

  private def astForElseClause(node: ElseClause): Ast = {
    val stmtList = ParserAst(node.thenClause).asInstanceOf[StatementList]
    astForStatementList(stmtList)
  }

  // May return an extra Ast for a LOCAL node if it doesn't exist already.
  protected def astForSingleAssignment(node: SingleAssignment): Seq[Ast] = {
    getAssignmentOperatorName(node.op) match
      case None =>
        logger.warn(s"Unrecognized assignment operator: ${node.text}, skipping")
        astForUnknown(node) :: Nil
      case Some(op) =>
        val lhs           = astForExpression(node.lhs)
        val rhs           = astForExpression(node.rhs)
        val call          = callNode(node, node.text, op, op, DispatchTypes.STATIC_DISPATCH)
        val assignmentAst = callAst(call, Seq(lhs, rhs))
        scope.lookupVariable(node.lhs.getText) match
          case None =>
            // We are introducing a new variable. Thus, also create a LOCAL node.
            val local = localNode(ParserAst(node.lhs), node.lhs.getText, node.lhs.getText, "", None)
            scope.addToScope(node.lhs.getText, local)
            Seq(Ast(local), assignmentAst)
          case Some(_) =>
            // This variable is already in scope, so just emit an assignment.
            Seq(assignmentAst)
  }

  // x.y = 1 is lowered as x.y=(1), i.e. as calling `y=` on `x` with argument `1`.
  protected def astForAttributeAssignment(node: AttributeAssignment): Ast = {
    val call         = SimpleCall(node.ctx, node.ctx, List(node.argument))
    val memberAccess = MemberAccess(node.ctx, node.target, ".", node.methodName + "=")
    astForMemberCallWithoutBlock(call, memberAccess)
  }

  // until T do B is lowered as while !T do B
  protected def astForUntilStatement(node: UntilExpression): Ast = {
    val notCondition = astForExpression(UnaryExpression(node.condition, "!", node.condition))
    val body         = astsForStatement(node.body)
    whileAst(Some(notCondition), body)
  }

  protected def astForWhileStatement(node: WhileExpression): Ast = {
    val condition = astForExpression(node.condition)
    val body      = astsForStatement(node.body)
    whileAst(Some(condition), body)
  }

  private def getAssignmentOperatorName(op: String): Option[String] = {
    Map(
      "="  -> Operators.assignment,
      "+=" -> Operators.assignmentPlus,
      "-=" -> Operators.assignmentMinus,
      "*=" -> Operators.assignmentMultiplication
    ).get(op)
  }

}
