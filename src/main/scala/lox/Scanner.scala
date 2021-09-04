package lox

import scala.collection.mutable.ArrayBuffer

enum TokenType:
  case LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  COMMA,
  DOT,
  MINUS,
  PLUS,
  SEMICOLON,
  SLASH,
  STAR,
  BANG,
  BANG_EQUAL,
  EQUAL,
  EQUAL_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,
  IDENTIFIER,
  STRING,
  NUMBER,
  AND,
  CLASS,
  ELSE,
  FALSE,
  FUN,
  FOR,
  IF,
  NIL,
  OR,
  PRINT,
  RETURN,
  SUPER,
  THIS,
  TRUE,
  VAR,
  WHILE,
  EOF

case class Token(val tokenType: TokenType,
                 val lexeme: String,
                 val literal: Any = null,
                 val line: Int = -1):
  override def toString: String = s"${tokenType} ${lexeme} ${literal}"

class Scanner(val source: String,
              val tokens: ArrayBuffer[Token] = ArrayBuffer[Token]()):
  import TokenType.*

  private val keywords = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE
  )
  private var start = 0
  private var current = 0
  private var line = 1

  def scanTokens(): List[Token] =
    while (!isAtEnd()) {
      start = current
      scanToken()
    }

    tokens.append(Token(EOF, "", null, line))
    return tokens.toList

  private def isAtEnd() = current >= source.length

  private def advance() =
    val currentChar = source.charAt(current)
    current += 1
    currentChar

  private def addToken(t: TokenType): Unit = addToken(t, null)

  private def addToken(t: TokenType, literal: Any) =
    val text = source.substring(start, current)
    tokens.append(Token(t, text, literal, line))

  private def advanceIf(expected: Char): Boolean =
    if isAtEnd() then return false
    if source.charAt(current) != expected then return false
    current += 1
    true

  private def peek(): Char =
    if isAtEnd() then '\u0000' else source.charAt(current);

  private def string(): Unit =
    while (peek() != '"' && !isAtEnd()) {
      if peek() == '\n' then line += 1
      advance()
    }

    if (isAtEnd()) {
      Lox.error(line, "Unterminated string.")
      return
    }

    advance()
    val value = source.substring(start + 1, current - 1)
    addToken(STRING, value)

  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  private def number(): Unit =
    while isDigit(peek()) do advance()

    if (peek() == '.' && isDigit(peekNext())) {
      advance()

      while isDigit(peek()) do advance()
    }

    addToken(NUMBER, source.substring(start, current).toDouble)

  private def peekNext(): Char =
    if current + 1 >= source.length then '\u0000' else source.charAt(current + 1)

  private def isAlpha(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  private def isAlphaNumeric(c: Char): Boolean =
    isAlpha(c) || isDigit(c)

  private def identifier(): Unit =
    while isAlphaNumeric(peek()) do advance()
    val text = source.substring(start, current)
    val tokType = keywords.getOrElse(text, IDENTIFIER)
    addToken(tokType)

  private def scanToken() = advance() match
    case '(' => addToken(LEFT_PAREN)
    case ')' => addToken(RIGHT_PAREN)
    case '{' => addToken(LEFT_BRACE)
    case '}' => addToken(RIGHT_BRACE)
    case ',' => addToken(COMMA)
    case '.' => addToken(DOT)
    case '-' => addToken(MINUS)
    case '+' => addToken(PLUS)
    case ';' => addToken(SEMICOLON)
    case '*' => addToken(STAR)
    case '!' => addToken(if advanceIf('=') then BANG_EQUAL else BANG)
    case '=' => addToken(if advanceIf('=') then EQUAL_EQUAL else EQUAL)
    case '<' => addToken(if advanceIf('=') then LESS_EQUAL else LESS)
    case '>' => addToken(if advanceIf('=') then GREATER_EQUAL else GREATER)
    case '/' => if (advanceIf('/')) {
      while (peek() != '\n' && !isAtEnd()) advance()
    } else {
      addToken(SLASH)
    }
    case ' ' | '\r' | '\t' => {} // ignore WS
    case '\n' => line += 1
    case '"' => string()
    case c if isDigit(c) => number()
    case c if isAlpha(c) => identifier()
    case _ => Lox.error(line, "Unexpected character.")
