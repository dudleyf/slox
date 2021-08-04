package lox.tests

import lox.*

class TreePrinterTests extends TestCase :
  test("stringifies an expression") {
    var expression = BinaryExpr(
      UnaryExpr(
        Token(TokenType.MINUS, "-", null, 1),
        LiteralExpr(123)),
      Token(TokenType.STAR, "*", null, 1),
      GroupingExpr(
        LiteralExpr(45.67))
    )
    TreePrinter.print(expression) shouldEqual "(* (- 123) (group 45.67))"
  }
