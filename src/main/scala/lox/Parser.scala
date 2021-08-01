package lox

import scala.collection.mutable


/**
 * program     -> declaration* EOF ;
 * declaration -> varDecl | statement ;
 * varDecl     -> "var" IDENTIFIER ( "=" expression )? ";" ;
 * statement   -> exprStmt | printStmt ;
 * exprStmt    -> expression ";" ;
 * printStmt   -> "print" expression ";" ;
 * expression  -> equality ;
 * equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
 * comparison  -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
 * term        -> factor ( ( "=" | "+" ) factor )* ;
 * factor      -> unary ( ( "/" | "*" ) unary )* ;
 * unary       -> ( "!" | "-" ) unary | primary ;
 * primary     -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
 */
class Parser(val tokens: List[Token]):

  import TokenType.*

  class ParseError extends Exception

  var current = 0

  private def peek(): Token =
    tokens(current)

  private def isAtEnd(): Boolean =
    peek().tokenType == EOF

  private def advance(): Token =
    if !isAtEnd() then current += 1
    previous()

  private def previous(): Token =
    tokens(current - 1)

  private def check(tpe: TokenType): Boolean =
    if isAtEnd() then false else peek().tokenType == tpe

  private def matchTokens(tokenTypes: TokenType*): Boolean =
    for tpe <- tokenTypes do
      if (check(tpe)) {
        advance()
        return true
      }
    false

  private def consume(tokenType: TokenType, message: String): Token =
    if check(tokenType) then
      advance()
    else
      throw error(peek(), message)

  private def error(token: Token, message: String): Throwable =
    Lox.error(token, message)
    return ParseError()

  private def synchronize(): Unit =
    advance()
    while !isAtEnd() do
      if previous().tokenType == SEMICOLON then
        ()
      else
        peek().tokenType match
          case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => ()
          case _ => advance()

  // expression -> equality ;
  def expression(): Expr =
    equality()

  // equality -> comparison ( ( "!=" | "==" ) comparison )* ;
  def equality(): Expr =
    var expr = comparison()
    while matchTokens(BANG_EQUAL, EQUAL_EQUAL) do
      val operator = previous()
      val right = comparison()
      expr = BinaryExpr(expr, operator, right)
    expr

  // comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  def comparison(): Expr =
    var expr = term()
    while matchTokens(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) do
      val operator = previous()
      val right = term()
      expr = BinaryExpr(expr, operator, right)
    expr

  // term -> factor ( ( "=" | "+" ) factor )* ;
  def term(): Expr =
    var expr = factor()
    while matchTokens(EQUAL, PLUS) do
      val operator = previous()
      val right = factor()
      expr = BinaryExpr(expr, operator, right)
    expr

  // factor -> unary ( ( "/" | "*" ) unary )* ;
  def factor(): Expr =
    var expr = unary()
    while matchTokens(SLASH, STAR) do
      val operator = previous()
      val right = unary()
      expr = BinaryExpr(expr, operator, right)
    expr

  // unary -> ( "!" | "-" ) unary | primary ;
  def unary(): Expr =
    if matchTokens(BANG, MINUS) then
      val operator = previous()
      val right = unary()
      return UnaryExpr(operator, right)
    primary()

  // primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
  def primary(): Expr =
    if matchTokens(FALSE) then return LiteralExpr(false)
    if matchTokens(TRUE) then return LiteralExpr(true)
    if matchTokens(NIL) then return LiteralExpr(null)
    if matchTokens(NUMBER, STRING) then return LiteralExpr(previous().literal)
    if matchTokens(IDENTIFIER) then return VariableExpr(previous())
    if (matchTokens(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      return GroupingExpr(expr)
    }
    throw error(peek(), "Expect expression.")

  def statement(): Stmt =
    if matchTokens(PRINT) then printStatement() else expressionStatement()

  def printStatement(): Stmt =
    val value = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    PrintStmt(value)

  def expressionStatement(): Stmt =
    val expr = expression()
    consume(SEMICOLON, "Expext ';' after expression.")
    ExpressionStmt(expr)

  def declaration(): Stmt =
    try
      if matchTokens(VAR) then varDeclaration() else statement()
    catch
      case e : ParseError =>
        synchronize()
        null

  def varDeclaration(): Stmt =
    val name = consume(IDENTIFIER, "Expect variable name.")
    var initializer = if matchTokens(EQUAL) then expression() else null
    consume(SEMICOLON, "Expect ';' after variable declaration.")
    VarStmt(name, initializer)

  def parse(): List[Stmt] =
    var statements = mutable.ListBuffer[Stmt]()
    while !isAtEnd() do
      statements.addOne(declaration())
    statements.toList
