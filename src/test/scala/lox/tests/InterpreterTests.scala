package lox.tests

class InterpreterTests extends TestCase :

  test("evaluates a simple arithmetic expression") {
    execute("print 1+2;") shouldEqual ("3\n")
  }

  test("variable assignment") {
    execute("var x = 1;\nprint x;") shouldEqual("1\n")
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
    execute(src) shouldEqual(expect)
  }
