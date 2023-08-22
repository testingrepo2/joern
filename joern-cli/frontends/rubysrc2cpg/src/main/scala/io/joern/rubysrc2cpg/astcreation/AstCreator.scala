package io.joern.rubysrc2cpg.astcreation
import io.joern.rubysrc2cpg.parser.RubyParser
import io.joern.rubysrc2cpg.parser.RubyParser.*
import io.joern.rubysrc2cpg.utils.PackageContext
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.Defines.DynamicCallUnknownFullName
import io.joern.x2cpg.datastructures.{Global, Scope}
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, Defines as XDefines}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import java.io.File as JFile
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Using}

class AstCreator(
  protected val filename: String,
  global: Global,
  parser: ResourceManagedParser,
  packageContext: PackageContext,
  projectRoot: Option[String] = None
) extends AstCreatorBase(filename)
    with AstNodeBuilder[ParserRuleContext, AstCreator] {

  protected val scope: Scope[String, NewIdentifier, Unit] = new Scope()

  private val logger = LoggerFactory.getLogger(this.getClass)

  protected val classStack = mutable.Stack[String]()

  protected val packageStack = mutable.Stack[String]()

  /*
   * Stack of variable identifiers incorrectly identified as method identifiers
   * Each AST contains exactly one call or identifier node
   */
  protected val methodNameAsIdentifierStack = mutable.Stack[Ast]()

  protected val methodAliases       = mutable.HashMap[String, String]()
  protected val methodNameToMethod  = mutable.HashMap[String, nodes.NewMethod]()
  protected val methodDefInArgument = mutable.ListBuffer[Ast]()

  protected val typeDeclNameToTypeDecl = mutable.HashMap[String, nodes.NewTypeDecl]()

  protected val methodNamesWithYield = mutable.HashSet[String]()

  /*
   *Fake methods created from yield blocks and their yield calls will have this suffix in their names
   */
  protected val YIELD_SUFFIX = "_yield"

  /*
   * This is used to mark call nodes created due to yield calls. This is set in their names at creation.
   * The appropriate name wrt the names of their actual methods is set later in them.
   */
  protected val UNRESOLVED_YIELD = "unresolved_yield"

  protected val pathSep = "."

  protected val blockMethods = ListBuffer[Ast]()

  protected val relativeFilename: String =
    projectRoot.map(filename.stripPrefix).map(_.stripPrefix(JFile.separator)).getOrElse(filename)

  // The below are for adding implicit return nodes to methods

  // This is true if the last statement of a method is being processed. The last statement could be a if-else as well
  protected var processingLastMethodStatement = false
  // a monotonically increasing block id unique within this file
  protected var blockIdCounter = 1
  // block id of the block currently being processed
  protected var currentBlockId = 0
  /*
   * This is a hash of parent block id ---> child block id. If there are multiple children, any one child can be present.
   * The value of this entry for a block is read AFTER its last statement has been processed. Absence of the the block
   * in this hash implies this is a leaf block.
   */
  protected val blockChildHash = mutable.HashMap[Int, Int]()

  protected val builtInCallNames = mutable.HashSet[String]()
  // Hashmap to store used variable names, to avoid duplicates in case of un-named variables
  protected val usedVariableNames = mutable.HashMap.empty[String, Int]

  protected def createIdentifierWithScope(
    ctx: ParserRuleContext,
    name: String,
    code: String,
    typeFullName: String,
    dynamicTypeHints: Seq[String] = Seq()
  ): NewIdentifier = {
    val newNode = identifierNode(ctx, name, code, typeFullName, dynamicTypeHints)
    scope.addToScope(name, newNode)
    newNode
  }

  protected def getActualMethodName(name: String): String = {
    methodAliases.getOrElse(name, name)
  }

  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    parser.parse(filename) match {
      case Success(programCtx) =>
        diffGraph
      case Failure(exc) =>
        logger.warn(s"Could not parse file: $filename, skipping")
        logger.warn(exc.getMessage)
        diffGraph
    }
  }

  override protected def line(node: ParserRuleContext): Option[Integer] = ???

  override protected def column(node: ParserRuleContext): Option[Integer] = ???

  override protected def lineEnd(node: ParserRuleContext): Option[Integer] = ???

  override protected def columnEnd(element: ParserRuleContext): Option[Integer] = ???
}
