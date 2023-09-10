package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.parser.ParserAst.*
import io.shiftleft.codepropertygraph.generated.PropertyNames

trait AstCreatorHelper { this: AstCreator =>

  override def column(node: ParserNode): Option[Integer] = node.column

  override def line(node: ParserNode): Option[Integer] = node.line

  override def columnEnd(node: ParserNode): Option[Integer] = node.columnEnd

  import GlobalTypes.*

  override def lineEnd(node: ParserNode): Option[Integer] = node.lineEnd

  def isBuiltin(x: String): Boolean = builtinFunctions.contains(x)

  def prefixAsBuiltin(x: String): String = s"$builtinPrefix$pathSep$x"

  def pathSep = "."

  protected def getEnclosingAstType: String = methodAstParentStack.head.label()

  // TODO: decide on the proper full name format.
  protected def computeClassFullName(name: String): String  = s"$getEnclosingAstFullName.$name"
  protected def computeMethodFullName(name: String): String = s"$getEnclosingAstFullName:$name"

  protected def getEnclosingAstFullName: String = methodAstParentStack.head.properties(PropertyNames.FULL_NAME).toString

}

object GlobalTypes {
  val builtinPrefix = "__builtin"
  /* Sources:
   * https://ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/function.html
   * https://ruby-doc.org/3.2.2/Kernel.html
   *
   * We comment-out methods that require an explicit "receiver" (target of member access.)
   */
  val builtinFunctions = Set(
    "Array",
    "Complex",
    "Float",
    "Hash",
    "Integer",
    "Rational",
    "String",
    "__callee__",
    "__dir__",
    "__method__",
    "abort",
    "at_exit",
    "autoload",
    "autoload?",
    "binding",
    "block_given?",
    "callcc",
    "caller",
    "caller_locations",
    "catch",
    "chomp",
    "chomp!",
    "chop",
    "chop!",
    // "class",
    // "clone",
    "eval",
    "exec",
    "exit",
    "exit!",
    "fail",
    "fork",
    "format",
    // "frozen?",
    "gets",
    "global_variables",
    "gsub",
    "gsub!",
    "iterator?",
    "lambda",
    "load",
    "local_variables",
    "loop",
    "open",
    "p",
    "print",
    "printf",
    "proc",
    "putc",
    "puts",
    "raise",
    "rand",
    "readline",
    "readlines",
    "require",
    "require_relative",
    "select",
    "set_trace_func",
    "sleep",
    "spawn",
    "sprintf",
    "srand",
    "sub",
    "sub!",
    "syscall",
    "system",
    "tap",
    "test",
    // "then",
    "throw",
    "trace_var",
    // "trap",
    "untrace_var",
    "warn"
    // "yield_self",
  )
}
