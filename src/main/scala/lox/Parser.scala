package lox

import scala.collection.mutable


/**
 * program     -> declaration* EOF ;
 * declaration -> varDecl | statement ;
 * varDecl     -> "var" IDENTIFIER ( "=" expression )? ";" ;
 * statement   -> exprStmt | ifStmt | printStmt | block ;
 * ifStmt      -> "if" "(" expression ")" statement ( "else" statement )? ;
 * block       -> "{" declaration* "}" ;
 * exprStmt    -> expression ";" ;
 * printStmt   -> "print" expression ";" ;
 * expression  -> assignment ;
 * assignment  -> IDENTIFIER "=" assignment | logic_or ;
 * logic_or    -> logic_and ( "or" logic_and )* ;
 * logic_and   -> equality ( "and" equality )* ;
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

  def expression(): Expr =
    assignment()

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
      UnaryExpr(operator, right)
    else
      primary()

  // primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
  def primary(): Expr =
    if matchTokens(FALSE) then LiteralExpr(false)
    else if matchTokens(TRUE) then LiteralExpr(true)
    else if matchTokens(NIL) then LiteralExpr(null)
    else if matchTokens(NUMBER, STRING) then LiteralExpr(previous().literal)
    else if matchTokens(IDENTIFIER) then VariableExpr(previous())
    else if (matchTokens(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      GroupingExpr(expr)
    }
    else throw error(peek(), "Expect expression.")

  def statement(): Stmt =
    if matchTokens(IF) then
      ifStatement()
    else if matchTokens(PRINT) then
      printStatement()
    else if matchTokens(LEFT_BRACE) then
      BlockStmt(block())
    else
      expressionStatement()

  def ifStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after if condition.")
    val thenBranch = statement()
    val elseBranch = if matchTokens(ELSE) then statement() else null
    IfStmt(condition, thenBranch, elseBranch)

  def printStatement(): Stmt =
    val value = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    PrintStmt(value)

  def expressionStatement(): Stmt =
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
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

  def assignment(): Expr =
    val expr = or()

    if matchTokens(EQUAL) then
      val equals = previous()
      val value = assignment()

      expr match
        case v: VariableExpr =>
          val name = v.name
          AssignExpr(name, value)
        case _ =>
          error(equals, "Invalid assignment target.")
          expr
    else
      expr

  def block(): List[Stmt] =
    var statements = mutable.ListBuffer.empty[Stmt]
    while !check(RIGHT_BRACE) && !isAtEnd() do
      statements.addOne(declaration())
    consume(RIGHT_BRACE, "Expect '}' after block.")
    statements.toList

  def or(): Expr =
    var expr = and()
    while matchTokens(OR) do
      var operator = previous()
      var right = and()
      expr = LogicalExpr(expr, operator, right)
    expr

  def and(): Expr =
    var expr = equality()
    while matchTokens(AND) do
      var operator = previous()
      var right = equality()
      expr = LogicalExpr(expr, operator, right)
    expr

  def parse(): List[Stmt] =
    var statements = mutable.ListBuffer[Stmt]()
    while !isAtEnd() do
      statements.addOne(declaration())
    statements.toList

