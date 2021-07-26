import jdk.incubator.vector.VectorOperators.Test
import lox.{AstPrinter, Binary, Expr, Grouping, Literal, Parser, Scanner, Token, TokenType, Unary}
import org.scalatest.*
import flatspec.*
import matchers.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

class ParserTests extends AnyFlatSpec with should.Matchers:
  def parse(source: String): Expr =
    val scanner = Scanner(source)
    val tokens =  scanner.scanTokens()
    val parser = Parser(tokens)
    parser.parse()

  it should "parse a simple expression" in {
    val expr = parse("1 + 2")
    AstPrinter.print(expr) shouldEqual "(+ 1.0 2.0)"
  }

class Tests extends AnyFunSpec:
  import TokenType._

  def scan(source: String): List[Token] =
    val scanner = Scanner(source)
    scanner.scanTokens()

  describe("Scanner") {
    it("scans a string of valid tokens") {
      val tokens = scan("{}!")
      assert(tokens(0) == Token(LEFT_BRACE, "{", null, 1))
      assert(tokens(1) == Token(RIGHT_BRACE, "}", null, 1))
      assert(tokens(2) == Token(BANG, "!", null, 1))
    }

    it("scans a number") {
      val tokens = scan("234.34")
      assert(tokens(0) == Token(NUMBER, "234.34", 234.34, 1))
    }
  }

  describe("AstPrinter") {
    it("stringifies an expression") {
      var expression = Binary(
        Unary(
          Token(TokenType.MINUS, "-", null, 1),
          Literal(123)),
        Token(TokenType.STAR, "*", null, 1),
        Grouping(
          Literal(45.67))
      )
      assert(AstPrinter.print(expression) == "(* (- 123) (group 45.67))")
    }
  }
