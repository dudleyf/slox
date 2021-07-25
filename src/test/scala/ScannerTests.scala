import jdk.incubator.vector.VectorOperators.Test
import lox.{Binary, Grouping, Literal, Token, TokenType, Unary, AstPrinter}
import org.scalatest.funspec.AnyFunSpec

class ScannerTests extends AnyFunSpec:
  import TokenType._

  def scan(source: String): List[Token] =
    val scanner = lox.Scanner(source)
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
      var printer = AstPrinter()
      assert(printer.print(expression) == "(* (- 123) (group 45.67))")
    }
  }
