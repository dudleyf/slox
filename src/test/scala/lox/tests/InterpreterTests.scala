package lox.tests

import lox.Lox
import lox.tests.TestCase

class InterpreterTests extends TestCase:
  behavior of "A Lox interpreter"

  it should "evaluate a simple arithmetic expression" in {
    run("print 1+2;") shouldEqual ("3\n")
  }

  it should "evaluate a variable declaration" in {
    run("var x = 1;\nprint x;") shouldEqual ("1\n")
  }

  it should "evaluate a variable assignment" in {
    run("var x = 1;\nx = 2;\nprint x;") shouldEqual ("2\n")
  }

  it should "evaluate nested scopes with a single variable" in {
    val src =
      """var a = "global a";
        |{
        |  var a = "outer a";
        |  {
        |    var a = "inner a";
        |    print a;
        |  }
        |  print a;
        |}
        |print a;""".stripMargin
    val expect =
      """inner a
        |outer a
        |global a
        |""".stripMargin
    run(src) shouldEqual (expect)
  }

  it should "evaluate a scope with multiple variables" in {
    val src =
      """var a = "global a";
        |var b = "global b";
        |{
        |  var a = "outer a";
        |  var b = "outer b";
        |  print a;
        |  print b;
        |}
        |print a;
        |print b;""".stripMargin
    val expect =
      """outer a
        |outer b
        |global a
        |global b
        |""".stripMargin
    run(src) shouldEqual (expect)
  }

  it should "evaluate nested scopes with multiple variables" in {
    val src =
      """var a = "global a";
        |var b = "global b";
        |{
        |  var a = "outer a";
        |  var b = "outer b";
        |  {
        |    var a = "inner a";
        |    print a;
        |    print b;
        |  }
        |  print a;
        |  print b;
        |}
        |print a;
        |print b;""".stripMargin
    val expect =
      """inner a
        |outer b
        |outer a
        |outer b
        |global a
        |global b
        |""".stripMargin
    run(src) shouldEqual (expect)
  }

//  test("scopes 3") {
//    val src =
//      """var a = "global a";
//        |var b = "global b";
//        |var c = "global c";
//        |{
//        |  var a = "outer a";
//        |  var b = "outer b";
//        |  {
//        |    var a = "inner a";
//        |    print a;
//        |    print b;
//        |    print c;
//        |  }
//        |  print a;
//        |  print b;
//        |  print c;
//        |}
//        |print a;
//        |print b;
//        |print c;""".stripMargin
//    val expect =
//      """inner a
//        |outer b
//        |global c
//        |outer a
//        |outer b
//        |global c
//        |global a
//        |global b
//        |global c
//        |""".stripMargin
//    execute(src) shouldEqual (expect)
//  }
//

  it should "evaluate if statements" in {
    run("if (true) { print 1; } else { print 2; }") shouldEqual ("1\n")
    run("if (false) { print 1; } else { print 2; }") shouldEqual ("2\n")
  }

  it should "evaluate logical expressions" in {
    run("print \"hi\" or 2;") shouldEqual "hi\n"
    run("print nil or \"yes\";") shouldEqual "yes\n"
    run("print \"hi\" and false;") shouldEqual "false\n"
    run("print \"hi\" and true;") shouldEqual "true\n"
  }

  it should "evaluate a while loop" in {
    val source =
      """var x = 0;
        |while (x < 3) {
        |  print x;
        |  x = x + 1;
        |}""".stripMargin
    val expected =
      """0
        |1
        |2
        |""".stripMargin
    run(source) shouldEqual expected
  }

  it should "evaluate a for loop" in {
    val source =
      """for (var x = 0; x < 3; x = x + 1) {
        |  print x;
        |}""".stripMargin
    val expected =
      """0
        |1
        |2
        |""".stripMargin
    run(source) shouldEqual expected
  }

  it should "clock() function should return a number" in  {
    val source = "print clock();"
    val result = run(source);
    result.toDoubleOption shouldBe defined
  }

  it should "evaluate a user-defined function" in {
    val source =
      """fun sayHi(first, last) {
        |  print "Hi, " + first + " " + last + "!";
        |}
        |
        |sayHi("Dear", "Reader");""".stripMargin
    val expected = "Hi, Dear Reader!\n"
    run(source) shouldBe expected
  }

  it should "evaluate a return statement" in {
    val source =
      """fun foo() {
        |  return 1;
        |}
        |var x = foo();
        |print x;
        |""".stripMargin
    val expected = "1\n"
    run(source) shouldBe expected
  }

  it should "evaluate fib" in {
    val source =
      """fun fib(n) {
        |  if (n <= 1) return n;
        |  return fib(n - 2) + fib(n - 1);
        |}
        |
        |for (var i = 0; i < 10; i = i + 1) {
        |  print fib(i);
        |}""".stripMargin
    val expected =
      """0
        |1
        |1
        |2
        |3
        |5
        |8
        |13
        |21
        |34
        |""".stripMargin
    run(source) shouldBe expected
  }

  it should "evaluate local functions" in {
    val source =
      """fun makeCounter() {
        |      var i = 0;
        |      fun count() {
        |        i = i + 1;
        |        print i;
        |      }
        |
        |      return count;
        |    }
        |
        |    var counter = makeCounter();
        |    counter(); // "1".
        |    counter(); // "2".
        |""".stripMargin
    val expected = "1\n2\n"
    run(source) shouldBe expected
  }

  it should "evaluate a closure" in {
    val source =
      """var a = "global";
        |{
        |  fun showA() {
        |    print a;
        |  }
        |
        |  showA();
        |  var a = "block";
        |  showA();
        |}""".stripMargin
    val expected = "global\nglobal\n"
    run(source) shouldEqual expected
  }

  it should "evaluate both a while and for loop" in {
    val source =
      """{
        |  var x = 0;
        |  while (x < 3) {
        |      {print x;}
        |      x = x + 1;
        |  }
        |
        |  for (var x = 0; x < 3; x = x + 1) {
        |    print x;
        |  }
        |}""".stripMargin
    val expected =
      """0
        |1
        |2
        |0
        |1
        |2
        |""".stripMargin
    run(source) shouldEqual expected
  }

  it should "desugar a for loop into an equivalent while loop" in {
    val whileSource =
      """{
        |  var x = 0;
        |  while (x < 3) {
        |      {print x;}
        |      x = x + 1;
        |  }
        |}""".stripMargin

    val forSource =
      """for (var x = 0; x < 3; x = x + 1) {
        |  print x;
        |}
        |""".stripMargin

    val lox = Lox()
    val whileAst = lox.parse(whileSource)
    val forAst = lox.parse(forSource)

    whileAst.toString() should equal (forAst.toString())
  }
