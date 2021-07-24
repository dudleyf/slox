package lox

class Token(val tokType: TokenType,
            val lexeme: String,
            val literal: Any,
            val line: Int):
  override def toString: String = s"${tokType} ${lexeme} ${literal}"

