package lox

import lox.Lox.interpreter

class InterpreterTests extends TestCase :
  import TestLox.*

  test("evaluates a simple arithmetic expression") {
    eval("print 1+2;") shouldEqual ("3\n")
  }

  test("variable assignment") {
    eval("var x = 1;\nprint x;") shouldEqual("1\n")
  }

  test ("scopes") {
    val src = """
        |var a = "global a";
        |var b = "global b";
        |var c = "global c";
        |{
        |  var a = "outer a";
        |  var b = "outer b";
        |  {
        |    var a = "inner a";
        |    print a;
        |    print b;
        |    print c;
        |  }
        |  print a;
        |  print b;
        |  print c;
        |}
        |print a;
        |print b;
        |print c;""".stripMargin
    val expect =
    """inner a
        |outer b
        |global c
        |outer a
        |outer b
        |global c
        |global a
        |global b
        |global c
        |""".stripMargin
    eval(src) shouldEqual(expect)
  }
