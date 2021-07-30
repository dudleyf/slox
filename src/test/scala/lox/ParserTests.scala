package lox

import lox.TestLox.parse

class ParserTests extends TestCase :
  test("parses a simple expression") {
    val expr = parse("1 + 2")
    TreePrinter.print(expr) shouldEqual "(+ 1.0 2.0)"
  }
