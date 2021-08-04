package lox.tests

import lox._

class ParserTests extends TestCase :
  test("parses a simple expression") {
    parse("1 + 2;") shouldEqual List(
      ExpressionStmt(
        BinaryExpr(
          LiteralExpr(1.0),
          Token(TokenType.PLUS, "+", null, 1),
          LiteralExpr(2.0))))
  }
