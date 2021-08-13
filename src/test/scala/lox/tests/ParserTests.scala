package lox.tests

import lox._

class ParserTests extends TestCase :
  test("parses a simple expression") {
    parse("1 + 2;") shouldEqual List(
      ExpressionStmt(
        BinaryExpr(
          LiteralExpr(Num(1.0)),
          Token(TokenType.PLUS, "+", null, 1),
          LiteralExpr(Num(2.0)))))
  }
