package io.joern.rubysrc2cpg.passes.ast

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{Operators, nodes}
import io.shiftleft.semanticcpg.language.*

class AssignCpgTests extends RubyCode2CpgFixture {

  "nested decomposing assign" should {
    val cpg = code("""x, (y, z) = [1, [2, 3]]""".stripMargin)

    def getSurroundingBlock: nodes.Block = {
      cpg.all.collect { case block: nodes.Block if block.code != "" => block }.head
    }

    "test block exists" in {
      // Throws if block does not exist.
      getSurroundingBlock
    }

    // TODO: .code property need to be fixed
    "test block node properties" ignore {
      val block = getSurroundingBlock
      block.code shouldBe
        """tmp0 = list
          |x = tmp0[0]
          |y = tmp0[1][0]
          |z = tmp0[1][1]""".stripMargin
      block.lineNumber shouldBe Some(1)
    }

    // TODO: Need to fix the local variables
    "test local node" ignore {
      cpg.method.name("Test0.rb::program").local.name("tmp0").headOption should not be empty
    }

    "test tmp variable assignment" in {
      val block         = getSurroundingBlock
      val tmpAssignNode = block.astChildren.isCall.sortBy(_.order).head
      // tmpAssignNode.code shouldBe "tmp0 = list"
      tmpAssignNode.methodFullName shouldBe Operators.assignment
      tmpAssignNode.lineNumber shouldBe Some(1)
    }

    // TODO: Fix the code property of the Block node & the order too
    "test assignments to targets" ignore {
      val block       = getSurroundingBlock
      val assignNodes = block.astChildren.isCall.sortBy(_.order).tail
      assignNodes.map(_.code) should contain theSameElementsInOrderAs List(
        "x = tmp0[0]",
        "y = tmp0[1][0]",
        "z = tmp0[1][1]"
      )
      assignNodes.map(_.lineNumber.get) should contain theSameElementsInOrderAs List(1, 1, 1)
    }

  }

  "array destructuring assign" should {
    val cpg = code("""x, *, y =  [1, 2, 3, 5]""".stripMargin)

    def getSurroundingBlock: nodes.Block = {
      cpg.all.collect { case block: nodes.Block if block.code != "" => block }.head
    }

    "test block exists" in {
      // Throws if block does not exist.
      getSurroundingBlock
    }

    // TODO: .code property need to be fixed
    "test block node properties" ignore {
      val block = getSurroundingBlock
      block.code shouldBe
        """tmp0 = list
          |x = tmp0[0]
          |y = tmp0[1][0]
          |z = tmp0[1][1]""".stripMargin
      block.astChildren.length shouldBe 4
      cpg.identifier("x").isEmpty shouldBe false
      cpg.identifier("y").isEmpty shouldBe false
      cpg.identifier("z").isEmpty shouldBe false

    }

    // TODO: Need to fix the local variables
    "test local node" ignore {
      cpg.method.name("Test0.rb::program").local.name("tmp0").headOption should not be empty
    }

  }

  "empty array assignment" should {
    val cpg = code("""x.y = []""".stripMargin)

    "have an empty assignment" in {
      val List(assignment) = cpg.call.name(Operators.assignment).l
      assignment.argument.where(_.argumentIndex(2)).isCall.name.l shouldBe List(Operators.arrayInitializer)
      assignment.argument.where(_.argumentIndex(2)).isCall.argument.l shouldBe List()
    }
  }
}
