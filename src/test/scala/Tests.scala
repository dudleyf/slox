import jdk.incubator.vector.VectorOperators.Test
import lox.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*

abstract class Tests extends AnyFlatSpec with should.Matchers

class ParserTests extends Tests :
  def parse(source: String): Expr =
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()
    val parser = Parser(tokens)
    parser.parse()

  "A parser" should "parse a simple expression" in {
    val expr = parse("1 + 2")
    AstPrinter.print(expr) shouldEqual "(+ 1.0 2.0)"
  }

class ScannerTests extends Tests :

  import TokenType.*

  def scan(source: String): List[Token] =
    val scanner = Scanner(source)
    scanner.scanTokens()

  "A scanner" should "scan a string of valid tokens" in {
    val tokens = scan("{}!")
    tokens(0) shouldEqual Token(LEFT_BRACE, "{", null, 1)
    tokens(1) shouldEqual Token(RIGHT_BRACE, "}", null, 1)
    tokens(2) shouldEqual Token(BANG, "!", null, 1)
  }

  "A scanner" should "scan a number" in {
    val tokens = scan("234.34")
    tokens(0) shouldEqual Token(NUMBER, "234.34", 234.34, 1)
  }

class AstPrinterTests extends Tests :
  "AstPrinter" should "stringify an expression" in {
    var expression = Binary(
      Unary(
        Token(TokenType.MINUS, "-", null, 1),
        Literal(123)),
      Token(TokenType.STAR, "*", null, 1),
      Grouping(
        Literal(45.67))
    )
    AstPrinter.print(expression) shouldEqual "(* (- 123) (group 45.67))"
  }

