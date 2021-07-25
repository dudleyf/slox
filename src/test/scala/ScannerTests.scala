import jdk.incubator.vector.VectorOperators.Test
import lox.TokenType
import lox.Token
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
