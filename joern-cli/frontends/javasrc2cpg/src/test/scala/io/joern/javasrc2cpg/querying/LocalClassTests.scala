package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class LocalClassTests extends JavaSrcCode2CpgFixture {

  "calls to local class methods vs nested class methods should have the correct methodFullNames" in {
    val cpg = code(
      """
        |package foo;
        |
        |public class Foo {
        |
        |  class LocalClass {
        |    public void localMethod() {}
        |  }
        |
        |  private int member;
        |
        |  public void foo(int param) {
        |    class LocalClass {
        |      public void localMethod() {}
        |    }
        |
        |    LocalClass l = new LocalClass();
        |    l.localMethod();
        |  }
        |
        |  public void bar(int param) {
        |    LocalClass l = new LocalClass();
        |    l.localMethod();
        |  }
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").call.nameExact("<init>").l) { case List(method) =>
      method.methodFullName shouldBe "foo.LocalClass.<init>:void()"
    }

    inside(cpg.method.name("foo").call.nameExact("localMethod").l) { case List(method) =>
      method.methodFullName shouldBe "foo.LocalClass.localMethod:void()"
    }

    inside(cpg.method.name("bar").call.nameExact("<init>").l) { case List(method) =>
      method.methodFullName shouldBe "foo.LocalClass.<init>:void()"
    }

    inside(cpg.method.name("bar").call.nameExact("localMethod").l) { case List(method) =>
      method.methodFullName shouldBe "foo.LocalClass.localMethod:void()"
    }
  }

  "calls to local class methods should have the correct methodFullNames" in {
    val cpg = code(
      """
        |package foo;
        |
        |public class Foo {
        |
        |  private int member;
        |
        |  public void foo(int param) {
        |    class LocalClass {
        |      public void localMethod() {}
        |    }
        |
        |    LocalClass l = new LocalClass();
        |    l.localMethod();
        |  }
        |}
        |""".stripMargin)

    inside(cpg.call.nameExact("<init>").l) { case List(method) =>
      method.methodFullName shouldBe "foo.LocalClass.<init>:void()"
    }

    inside(cpg.call.nameExact("localMethod").l) { case List(method) =>
      method.methodFullName shouldBe "foo.LocalClass.localMethod:void()"
    }
  }
}
