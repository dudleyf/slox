package lox.tests

import lox.TokenType.{BANG, LEFT_BRACE, NUMBER, RIGHT_BRACE}
import lox.{Lox, Token, TokenType}
import lox.tests.TestCase

class ScannerTests extends TestCase :
  "A scanner" should "scan a string of valid tokens" in {
    val tokens = Lox().scan("{}!")
    tokens(0) shouldEqual Token(LEFT_BRACE, "{", null, 1)
    tokens(1) shouldEqual Token(RIGHT_BRACE, "}", null, 1)
    tokens(2) shouldEqual Token(BANG, "!", null, 1)
  }

  "A scanner" should "scan a number" in {
    val tokens = Lox().scan("234.34")
    tokens(0) shouldEqual Token(NUMBER, "234.34", 234.34, 1)
  }
