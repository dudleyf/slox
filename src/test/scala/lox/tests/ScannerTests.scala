package lox.tests

import lox.TokenType.{BANG, LEFT_BRACE, NUMBER, RIGHT_BRACE}
import lox.{Token, TokenType}

class ScannerTests extends TestCase :
  test("A scanner scans a string of valid tokens") {
    val tokens = scan("{}!")
    tokens(0) shouldEqual Token(LEFT_BRACE, "{", null, 1)
    tokens(1) shouldEqual Token(RIGHT_BRACE, "}", null, 1)
    tokens(2) shouldEqual Token(BANG, "!", null, 1)
  }

  test("A scanner scans a number") {
    val tokens = scan("234.34")
    tokens(0) shouldEqual Token(NUMBER, "234.34", 234.34, 1)
  }
