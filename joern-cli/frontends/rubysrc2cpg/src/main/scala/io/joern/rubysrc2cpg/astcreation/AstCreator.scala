package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.parser.ParserAst.*
import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.parser.{ParserAst, RubyParser}
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.getBuiltInType
import io.joern.rubysrc2cpg.utils.PackageContext
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.{Global, Scope}
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewLocal, NewNamespaceBlock, NewNode}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.util.{Failure, Success}

class AstCreator(
  protected val filename: String,
  global: Global,
  parser: ResourceManagedParser,
  packageContext: PackageContext,
  projectRoot: Option[String] = None
)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase(filename)
    with AstForStatementsCreator
    with AstForExpressionsCreator
    with AstForFunctionsCreator
    with AstForTypesCreator
    with AstCreatorHelper
    with AstNodeBuilder[ParserNode, AstCreator] {

  protected val logger = LoggerFactory.getLogger(this.getClass)

  // Used to compute a method's fullName and parent. Pushes NAMESPACE_BLOCKs and METHODs
  protected val methodAstParentStack: Stack[NewNode] = new Stack()

  // Used to track variable names and their LOCAL nodes.
  protected val scope: Scope[String, NewNode, NewNode] = new Scope()

  // All fileName fields should be relative to the scanned project's root.
  protected val relativeFilename: String =
    projectRoot.map(filename.stripPrefix).map(_.stripPrefix(java.io.File.separator)).getOrElse(filename)

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    parser.parse(filename) match {
      case Success(programCtx) =>
        val rootNode = ParserAst(programCtx).asInstanceOf[StatementList]
        val ast      = astForRubyFile(rootNode)
        storeInDiffGraph(ast, diffGraph)
        diffGraph
      case Failure(exc) =>
        logger.warn(s"Could not parse file: $filename, skipping")
        logger.warn(exc.getMessage)
        diffGraph
    }
  }

  // A Ruby file has the following AST structure: FILE -> NAMESPACE_BLOCK -> METHOD.
  // The (parsed) contents of the file are put under that fictitious METHOD node, thus
  // allowing for a straightforward representation of out-of-method statements.
  private def astForRubyFile(rootNode: StatementList): Ast = {
    val fileNode = NewFile().name(relativeFilename)
    val namespaceBlock = NewNamespaceBlock()
      .filename(relativeFilename)
      .name(NamespaceTraversal.globalNamespaceName)
      .fullName(s"$relativeFilename:${NamespaceTraversal.globalNamespaceName}")

    methodAstParentStack.push(namespaceBlock)
    scope.pushNewScope(namespaceBlock)
    val rubyFileMethod = astInFakeMethod(rootNode)
    scope.popScope()
    methodAstParentStack.pop()

    Ast(fileNode).withChild(Ast(namespaceBlock).withChild(rubyFileMethod))
  }

  private def astInFakeMethod(rootNode: StatementList): Ast = {
    val name      = ":program" // TODO: avoid this hardcoding
    val fullName  = computeMethodFullName(name)
    val signature = None       // TODO: should be set?
    val code      = rootNode.text
    val methodNode_ =
      methodNode(
        rootNode,
        name,
        code,
        fullName,
        signature,
        relativeFilename,
        Some(getEnclosingAstType),
        Some(getEnclosingAstFullName)
      )
    val methodReturn = methodReturnNode(rootNode, Defines.Any)

    methodAstParentStack.push(methodNode_)
    scope.pushNewScope(methodNode_)

    val globalVariableAsts = introduceGlobalVariablesAsLocals()
    val stmtAsts           = rootNode.statements.flatMap(astsForStatement)
    val bodyAst            = blockAst(blockNode(rootNode), globalVariableAsts ++ stmtAsts)

    scope.popScope()
    methodAstParentStack.pop()

    methodAst(methodNode_, Seq.empty, bodyAst, methodReturn)
  }

  // Ruby's predefined variables are introduced as LOCALs for each file.
  // Source: https://rubyreferences.github.io/rubyref/language/globals.html
  private def introduceGlobalVariablesAsLocals(): List[Ast] = {
    val globalVariables =
      Map(
        "$!"                  -> Defines.Any,
        "$@"                  -> Defines.Any,
        "$~"                  -> Defines.Any,
        "$&"                  -> Defines.String,
        "$`"                  -> Defines.String,
        "$'"                  -> Defines.String,
        "$+"                  -> Defines.Any,
        "$1"                  -> Defines.Any,
        "$2"                  -> Defines.Any,
        "$3"                  -> Defines.Any,
        "$/"                  -> Defines.String,
        "$\\"                 -> Defines.String,
        "$,"                  -> Defines.String,
        "$;"                  -> Defines.String,
        "$."                  -> Defines.Integer,
        "$<"                  -> Defines.Any, // TODO: ARGF class
        "$>"                  -> Defines.Any, // TODO: IO class
        "$_"                  -> Defines.String,
        "$0"                  -> Defines.String,
        "$*"                  -> Defines.Array,
        "$$"                  -> Defines.Integer,
        "$?"                  -> Defines.Integer,
        "$LOAD_PATH"          -> Defines.Array,
        "$LOADED_FEATURES"    -> Defines.Array,
        "$DEBUG"              -> Defines.Any,
        "$FILENAME"           -> Defines.String,
        "$stderr"             -> Defines.Any, // TODO: IO class
        "$stdout"             -> Defines.Any, // TODO: IO class
        "$stdin"              -> Defines.Any, // TODO: IO class
        "$VERBOSE"            -> Defines.Any,
        "$-a"                 -> Defines.Any,
        "$-i"                 -> Defines.Any,
        "$-l"                 -> Defines.Any,
        "$-p"                 -> Defines.Any,
        "STDIN"               -> Defines.Any,
        "STDOUT"              -> Defines.Any,
        "STDERR"              -> Defines.Any,
        "ENV"                 -> Defines.Hash,
        "ARGF"                -> Defines.Any,
        "ARGV"                -> Defines.Any,
        "DATA"                -> Defines.String,
        "TOPLEVEL_BINDING"    -> Defines.Any,
        "RUBY_VERSION"        -> Defines.String,
        "RUBY_RELEASE_DATE"   -> Defines.String,
        "RUBY_PLATFORM"       -> Defines.String,
        "RUBY_PATCHLEVEL"     -> Defines.String,
        "RUBY_REVISION"       -> Defines.String,
        "RUBY_COPYRIGHT"      -> Defines.String,
        "RUBY_ENGINE"         -> Defines.String,
        "RUBY_ENGINE_VERSION" -> Defines.String,
        "RUBY_DESCRIPTION"    -> Defines.String
      )

    globalVariables.map { case (variableName, typeName) =>
      val local = NewLocal().name(variableName).typeFullName(getBuiltInType(typeName)).code(variableName)
      scope.addToScope(variableName, local)
      Ast(local)
    }.toList
  }

}
