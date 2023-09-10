package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser.RubyParser.{QuotedExpandedStringArrayLiteralContext, QuotedExpandedSymbolArrayLiteralContext}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.misc.Interval

/** A simplified representation of ANTLR's concrete parse tree for Ruby, thus considerably reducing the amount of
  * constructs the AstCreator must handle. The shape of the AST is up to us and can be easily modified at will. Anything
  * for which there's no representation simply becomes an `Unknown`.
  */
object ParserAst {

  def apply(ctx: ParserRuleContext): ParserNode = {
    ParserAstCreator.translate(ctx)
  }

  abstract class ParserNode(ctx: ParserRuleContext) {
    def line: Option[Integer]      = Option(ctx.getStart.getLine)
    def column: Option[Integer]    = Option(ctx.getStart.getCharPositionInLine)
    def lineEnd: Option[Integer]   = Option(ctx.getStop.getLine)
    def columnEnd: Option[Integer] = Option(ctx.getStop.getCharPositionInLine)
    def text: String =
      ctx.getStart.getInputStream.getText(new Interval(ctx.getStart.getStartIndex, ctx.getStop.getStopIndex))
  }

  //
  // Fallback to be used when we don't know how to recognize a given parse tree node
  //
  final case class Unknown(ctx: ParserRuleContext) extends ParserNode(ctx)

  //
  // Sequence of statements
  //
  final case class StatementList(ctx: ParserRuleContext, statements: List[ParserRuleContext]) extends ParserNode(ctx) {
    override def text: String = {
      if (statements.size == 1) ctx.getText else "(...)" // Otherwise the entire interval of text would be used
    }
  }

  //
  // Declarations
  //
  final case class ModuleDeclaration(ctx: ParserRuleContext, moduleName: ParserRuleContext, body: ParserRuleContext)
      extends ParserNode(ctx)

  final case class ClassDeclaration(
    ctx: ParserRuleContext,
    className: ParserRuleContext,
    baseClass: Option[ParserRuleContext],
    body: ParserRuleContext
  ) extends ParserNode(ctx) {
    override def text: String = s"class ${className.getText} (...)"
  }
  final case class MethodDeclaration(
    ctx: ParserRuleContext,
    methodName: String,
    parameters: List[ParserRuleContext],
    body: ParserRuleContext
  ) extends ParserNode(ctx) {
    override def text: String = s"def $methodName (...)"
  }
  final case class SingletonMethodDeclaration(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    methodName: String,
    parameters: List[ParserRuleContext],
    body: ParserRuleContext
  ) extends ParserNode(ctx) {
    // TODO: the proper name could be rendered using either . or ::
    override def text: String = s"def $target.$methodName (...)"
  }
  final case class FieldDeclarations(ctx: ParserRuleContext, fields: List[ParserRuleContext]) extends ParserNode(ctx) {
    def hasGetter: Boolean = ctx.getText.startsWith("attr_reader") || ctx.getText.startsWith("attr_accessor")
    def hasSetter: Boolean = ctx.getText.startsWith("attr_writer") || ctx.getText.startsWith("attr_accessor")
  }

  //
  // Parameters
  //
  final case class MandatoryParameter(ctx: ParserRuleContext) extends ParserNode(ctx)
  final case class OptionalParameter(ctx: ParserRuleContext, name: ParserRuleContext, defaultExpr: ParserRuleContext)
      extends ParserNode(ctx)
  final case class ArrayParameter(ctx: ParserRuleContext, name: Option[String])   extends ParserNode(ctx)
  final case class HashParameter(ctx: ParserRuleContext, name: Option[String])    extends ParserNode(ctx)
  final case class ProcParameter(ctx: ParserRuleContext, name: ParserRuleContext) extends ParserNode(ctx)

  //
  // Assignments
  //
  final case class SingleAssignment(ctx: ParserRuleContext, lhs: ParserRuleContext, op: String, rhs: ParserRuleContext)
      extends ParserNode(ctx)

  /** Used to represent setter invocations, e.g. x.y = 1 */
  final case class AttributeAssignment(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    op: String,
    methodName: String,
    argument: ParserRuleContext
  ) extends ParserNode(ctx)

  //
  // Control structures
  //
  final case class WhileExpression(ctx: ParserRuleContext, condition: ParserRuleContext, body: ParserRuleContext)
      extends ParserNode(ctx)
  final case class UntilExpression(ctx: ParserRuleContext, condition: ParserRuleContext, body: ParserRuleContext)
      extends ParserNode(ctx)
  final case class IfExpression(
    ctx: ParserRuleContext,
    condition: ParserRuleContext,
    thenClause: ParserRuleContext,
    elsifClauses: List[ParserRuleContext],
    elseClause: Option[ParserRuleContext]
  ) extends ParserNode(ctx)
  final case class ElsIfClause(ctx: ParserRuleContext, condition: ParserRuleContext, thenClause: ParserRuleContext)
      extends ParserNode(ctx)
  final case class ElseClause(ctx: ParserRuleContext, thenClause: ParserRuleContext) extends ParserNode(ctx)
  final case class UnlessExpression(
    ctx: ParserRuleContext,
    condition: ParserRuleContext,
    trueBranch: ParserRuleContext,
    falseBranch: Option[ParserRuleContext]
  ) extends ParserNode(ctx)
  final case class ReturnExpression(ctx: ParserRuleContext, expressions: List[ParserRuleContext])
      extends ParserNode(ctx)
  final case class ConditionalExpression(
    ctx: ParserRuleContext,
    condition: ParserRuleContext,
    trueBranch: ParserRuleContext,
    falseBranch: ParserRuleContext
  ) extends ParserNode(ctx)

  //
  // Identifiers
  //
  /** Used to represent an unqualified identifier, e.g. X, x, @x, @@x, $x, $< */
  final case class SimpleIdentifier(ctx: ParserRuleContext, typeFullName: Option[String] = None) extends ParserNode(ctx)
  final case class SelfIdentifier(ctx: ParserRuleContext)                                        extends ParserNode(ctx)

  //
  // Literals
  //
  /** Used to represent non-interpolated literals of any kind */
  final case class StaticLiteralExpression(ctx: ParserRuleContext, typeFullName: String) extends ParserNode(ctx) {
    def isSymbol: Boolean = ctx.getText.startsWith(":")
    def isString: Boolean = ctx.getText.startsWith("\"")

    def innerText: String = {
      val originalText = ctx.getText
      if (originalText.startsWith(":'")) {
        originalText.drop(2).dropRight(1)
      } else if (originalText.startsWith(":")) {
        originalText.drop(1)
      } else if (originalText.startsWith("'")) {
        originalText.drop(1).dropRight(1)
      } else {
        originalText
      }
    }
  }

  /** Used to represent interpolated literals of any kind */
  final case class DynamicLiteralExpression(
    ctx: ParserRuleContext,
    typeFullName: String,
    expressions: List[ParserRuleContext]
  ) extends ParserNode(ctx)

  //
  // Ranges
  //
  final case class RangeExpression(ctx: ParserRuleContext, lowerBound: ParserRuleContext, upperBound: ParserRuleContext)
      extends ParserNode(ctx)

  //
  // Arrays
  //
  final case class ArrayLiteral(ctx: ParserRuleContext, elems: Seq[ParserRuleContext]) extends ParserNode(ctx) {
    def isSymbolArray: Boolean = ctx.getText.take(2).toLowerCase.startsWith("%i")
    def isStringArray: Boolean = ctx.getText.take(2).toLowerCase.startsWith("%w")
    def isDynamic: Boolean = ctx.isInstanceOf[QuotedExpandedSymbolArrayLiteralContext] ||
      ctx.isInstanceOf[QuotedExpandedStringArrayLiteralContext]
    def isStatic: Boolean = !isDynamic
  }

  //
  // Hashes
  //
  final case class HashLiteral(ctx: ParserRuleContext, elems: List[ParserRuleContext]) extends ParserNode(ctx)
  final case class Association(ctx: ParserRuleContext, key: ParserRuleContext, value: ParserRuleContext)
      extends ParserNode(ctx)

  //
  // Invocations
  //
  /** Used to represent traditional calls, e.g. foo; foo x, y; foo(x, y) */
  final case class SimpleCall(ctx: ParserRuleContext, target: ParserRuleContext, arguments: List[ParserRuleContext])
      extends ParserNode(ctx)

  /** Used to represent member calls, e.g. x.y(z, w) */
  final case class MemberCall(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    op: String,
    methodName: String,
    arguments: List[ParserRuleContext]
  ) extends ParserNode(ctx)

  /** Used to represent member calls with a do/braces block, e.g. x.y { .. }; x.z(1) do ... end */
  final case class MemberCallWithBlock(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    op: String,
    methodName: String,
    arguments: List[ParserRuleContext],
    block: ParserRuleContext
  ) extends ParserNode(ctx) {
    def withoutBlock: MemberCall = MemberCall(ctx, target, op, methodName, arguments)
  }

  /** Used to represent calls with a do/braces block, e.g. foo { ... }, foo x do ... end */
  final case class CallWithBlock(
    ctx: ParserRuleContext,
    target: ParserRuleContext,
    arguments: List[ParserRuleContext],
    block: ParserRuleContext
  ) extends ParserNode(ctx) {
    def withoutBlock: SimpleCall = SimpleCall(ctx, target, arguments)
  }

  //
  // Member/Index accesses
  //
  /** Use to represent index access, e.g. x[0], self.x.y[1, 2] */
  final case class IndexAccess(ctx: ParserRuleContext, target: ParserRuleContext, indexes: List[ParserRuleContext])
      extends ParserNode(ctx)

  /** Use to represent parameter-less member accesses, e.g. x.f, x.y.z, x::y&.z.w */
  final case class MemberAccess(ctx: ParserRuleContext, target: ParserRuleContext, op: String, methodName: String)
      extends ParserNode(ctx)

  //
  // Blocks
  //
  /** Used to represent do/braces blocks, e.g. {|z| z}, do |z| z end, do 1 end */
  final case class Block(ctx: ParserRuleContext, parameters: List[ParserRuleContext], body: ParserRuleContext)
      extends ParserNode(ctx)

  //
  // Unary/Binary expressions
  //
  final case class UnaryExpression(ctx: ParserRuleContext, op: String, expr: ParserRuleContext) extends ParserNode(ctx)
  final case class BinaryExpression(ctx: ParserRuleContext, lhs: ParserRuleContext, op: String, rhs: ParserRuleContext)
      extends ParserNode(ctx)
}
