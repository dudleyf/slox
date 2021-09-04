package lox.tests

import lox.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserTests extends TestCase :
  "parser" should "parse a simple expression" in {
    Lox().parse("1 + 2;") shouldEqual Seq(
      ExpressionStmt(
        BinaryExpr(
          LiteralExpr(Num(1.0)),
          Token(TokenType.PLUS, "+", null, 1),
          LiteralExpr(Num(2.0)))))
  }

  "parser" should "parse nested blocks" in {
    val source =
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
    val ast = Seq(
        VarStmt(Token(TokenType.IDENTIFIER, "a", null, 1), Some(LiteralExpr(Str("global a")))),
        VarStmt(Token(TokenType.IDENTIFIER, "b", null, 2), Some(LiteralExpr(Str("global b")))),
        BlockStmt(Seq(
          VarStmt(Token(TokenType.IDENTIFIER, "a", null, 4), Some(LiteralExpr(Str("outer a")))),
          VarStmt(Token(TokenType.IDENTIFIER, "b", null, 5), Some(LiteralExpr(Str("outer b")))),
          BlockStmt(Seq(
            VarStmt(Token(TokenType.IDENTIFIER, "a", null, 7), Some(LiteralExpr(Str("inner a")))),
            PrintStmt(VariableExpr(Token(TokenType.IDENTIFIER, "a", null, 8))),
            PrintStmt(VariableExpr(Token(TokenType.IDENTIFIER, "b", null, 9)))
          )),
          PrintStmt(VariableExpr(Token(TokenType.IDENTIFIER, "a", null, 11))),
          PrintStmt(VariableExpr(Token(TokenType.IDENTIFIER, "b", null, 12)))
        )),
        PrintStmt(VariableExpr(Token(TokenType.IDENTIFIER, "a", null, 14))),
        PrintStmt(VariableExpr(Token(TokenType.IDENTIFIER, "b", null, 15)))
      )
    Lox().parse(source) shouldEqual ast
  }
