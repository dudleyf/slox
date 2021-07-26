package lox

sealed abstract class Expr:
  def accept[T](visitor: Visitor[T]): T

trait Visitor[T]:
  def visit(expr: Binary): T
  def visit(expr: Grouping): T
  def visit(expr: Literal): T
  def visit(expr: Unary): T


case class Binary(left: Expr, operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this)

case class Grouping(expression: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this)

case class Literal(value: Any) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this);

case class Unary(operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: Visitor[T]): T = visitor.visit(this)


object AstPrinter extends Visitor[String]:
  override def visit(expr: Binary): String =
    parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visit(expr: Grouping): String =
    parenthesize("group", expr.expression)

  override def visit(expr: Literal): String =
    if expr.value == null then "nil" else expr.value.toString()

  override def visit(expr: Unary): String =
    parenthesize(expr.operator.lexeme, expr.right)

  def print(expr: Expr): String = expr.accept(this)

  def parenthesize(str: String, exprs: Expr*): String =
    var sb = StringBuilder()
    sb ++= "("
    sb ++= str
    sb ++= " "
    sb ++= exprs.map((x) => x.accept(this)).mkString(" ")
    sb ++= ")"
    sb.toString()


/**
 * expression -> equality ;
 * equality -> comparison ( ( "!=" | "==" ) comparison )* ;
 * comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
 * term -> factor ( ( "=" | "+" ) factor )* ;
 * factor -> unary ( ( "/" | "*" ) unary )* ;
 * unary -> ( "!" | "-" ) unary
 *       | primary ;
 * primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
 */
class Parser(val tokens: List[Token]):
  import TokenType._

  class ParseError extends Exception

  var current = 0

  private def peek(): Token =
    tokens(current)

  private def isAtEnd(): Boolean =
    peek().tokType == EOF

  private def advance(): Unit =
    if !isAtEnd() then current += 1
    previous()

  private def previous(): Token =
    tokens(current - 1)

  private def check(tpe: TokenType): Boolean =
    if isAtEnd() then false else peek().tokType == tpe

  private def matchTokens(tokenTypes: TokenType*): Boolean =
    for (tpe <- tokenTypes) {
      if (check(tpe)) {
        advance()
        return true
      }
    }
    false

  private def consume(tokenType: TokenType, message: String): Unit = {
    if (check(tokenType)) {
      return advance()
    } else {
      throw error(peek(), message)
    }
  }

  private def error(token: Token, message: String): Throwable =
    Lox.error(token, message)
    return ParseError()

  private def synchronize(): Unit =
    advance()
    while (!isAtEnd())
      if previous().tokType == SEMICOLON then return
      peek().tokType match {
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return
        case _ => advance()
      }

  // expression -> equality ;
  def expression(): Expr =
    equality()

  // equality -> comparison ( ( "!=" | "==" ) comparison )* ;
  def equality(): Expr =
    var expr = comparison()
    while (matchTokens(BANG_EQUAL, EQUAL_EQUAL)) {
      val operator = previous()
      val right = comparison()
      expr = Binary(expr, operator, right)
    }
    expr

  // comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  def comparison(): Expr =
    var expr = term()
    while (matchTokens(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous()
      val right = term()
      expr = Binary(expr, operator, right)
    }
    expr

  // term -> factor ( ( "=" | "+" ) factor )* ;
  def term(): Expr =
    var expr = factor()
    while (matchTokens(EQUAL, PLUS)) {
      val operator = previous()
      val right = factor()
      expr = Binary(expr, operator, right)
    }
    expr

  // factor -> unary ( ( "/" | "*" ) unary )* ;
  def factor(): Expr =
    var expr = unary()
    while (matchTokens(SLASH, STAR)) {
      val operator = previous()
      val right = unary()
      expr = Binary(expr, operator, right)
    }
    expr

  // unary -> ( "!" | "-" ) unary | primary ;
  def unary(): Expr =
    if (matchTokens(BANG, MINUS)) {
      val operator = previous()
      val right = unary()
      return Unary(operator, right)
    }
    primary()

  // primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
  def primary(): Expr =
    if matchTokens(FALSE) then return Literal(false)
    if matchTokens(TRUE) then return Literal(true)
    if matchTokens(NIL) then return Literal(null)
    if matchTokens(NUMBER, STRING) then return Literal(previous().literal)
    if (matchTokens(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      return Grouping(expr)
    }
    throw error(peek(), "Expect expression.")

  def parse(): Expr =
    try {
      return expression()
    } catch {
      case e => return null
    }
