package lox.tests

class InterpreterTests extends TestCase :
  test("evaluates a simple arithmetic expression") {
    execute("print 1+2;") shouldEqual ("3\n")
  }

  test("variable declaration") {
    execute("var x = 1;\nprint x;") shouldEqual ("1\n")
  }

  test("variable assignment") {
    var x = parse("var x = 1;\nx = 2;\nprint x;")
    execute("var x = 1;\nx = 2;\nprint x;") shouldEqual ("2\n")
  }

  test("scopes") {
    val src =
      """var a = "global a";
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
    execute(src) shouldEqual (expect)
  }

  test("if statement") {
    execute("if (true) { print 1; } else { print 2; }") shouldEqual ("1\n")
    execute("if (false) { print 1; } else { print 2; }") shouldEqual ("2\n")
  }

  test("logical expressions") {
    execute("print \"hi\" or 2;") shouldEqual "hi\n"
    execute("print nil or \"yes\";") shouldEqual "yes\n"
    execute("print \"hi\" and false;") shouldEqual "false\n"
    execute("print \"hi\" and true;") shouldEqual "true\n"
  }

  test("while loop") {
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
    execute(source) shouldEqual expected
  }

  test("for loop") {
    val source =
      """for (var x = 0; x < 3; x = x + 1) {
        |  print x;
        |}""".stripMargin
    val expected =
      """0
        |1
        |2
        |""".stripMargin
    execute(source) shouldEqual expected
  }

  test("clock() function") {
    val source = "print clock();"
    val result = execute(source);
    result.toDoubleOption shouldBe defined
  }

  test("user-defined functions") {
    val source =
      """fun sayHi(first, last) {
        |  print "Hi, " + first + " " + last + "!";
        |}
        |
        |sayHi("Dear", "Reader");""".stripMargin
    val expected = "Hi, Dear Reader!\n"
    execute(source) shouldBe expected
  }

  test("return") {
    val source =
      """fun foo() {
        |  return 1;
        |}
        |var x = foo();
        |print x;
        |""".stripMargin
    val expected = "1\n"
    execute(source) shouldBe expected
  }

  test("fib") {
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
    execute(source) shouldBe expected
  }

  test("local functions") {
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
    execute(source) shouldBe expected
  }
