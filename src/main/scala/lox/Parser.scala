package lox

import scala.collection.mutable


/**
 * program     -> declaration* EOF ;
 * declaration -> funDecl | varDecl | statement ;
 * funDecl     -> "fun" function ;
 * function    -> IDENTIFIER "(" parameters? ")" block ;
 * parameters  -> IDENTIFIER ( "," IDENTIFIER )* ;
 * varDecl     -> "var" IDENTIFIER ( "=" expression )? ";" ;
 * statement   -> exprStmt | forStmt | ifStmt | printStmt | returnStmt | whileStmt | block ;
 * returnStmt  -> "return" expression? ";" ;
 * forStmt     -> "for" "(" varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
 * whileStmt   -> "while" "(" expression ")" statement ;
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
 * unary       -> ( "!" | "-" ) unary | call ;
 * call        -> primary ( "(" arguments? ")" )* ;
 * arguments   -> expression ( "," expression )* ;
 * primary     -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
 */
class Parser(val tokens: List[Token]):
  import TokenType.*

  val MAX_ARGS = 255;

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

  def equality(): Expr =
    var expr = comparison()
    while matchTokens(BANG_EQUAL, EQUAL_EQUAL) do
      val operator = previous()
      val right = comparison()
      expr = BinaryExpr(expr, operator, right)
    expr

  def comparison(): Expr =
    var expr = term()
    while matchTokens(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) do
      val operator = previous()
      val right = term()
      expr = BinaryExpr(expr, operator, right)
    expr

  def term(): Expr =
    var expr = factor()
    while matchTokens(MINUS, PLUS) do
      val operator = previous()
      val right = factor()
      expr = BinaryExpr(expr, operator, right)
    expr

  def factor(): Expr =
    var expr = unary()
    while matchTokens(SLASH, STAR) do
      val operator = previous()
      val right = unary()
      expr = BinaryExpr(expr, operator, right)
    expr

  def unary(): Expr =
    if matchTokens(BANG, MINUS) then
      val operator = previous()
      val right = unary()
      UnaryExpr(operator, right)
    else
      call()

  def call(): Expr =
    var expr = primary()
    while matchTokens(LEFT_PAREN) do
      expr = finishCall(expr)
    expr

  def finishCall(callee: Expr): Expr =
    var arguments = mutable.ArrayBuffer.empty[Expr]
    if !check(RIGHT_PAREN) then
      while
        if arguments.size >= MAX_ARGS then error(peek(), "Can't have more than 255 arguments.")
        arguments.addOne(expression())
        matchTokens(COMMA)
      do ()
    val paren = consume(RIGHT_PAREN, "Expect ')' after arguments.")
    CallExpr(callee, paren, arguments.toList)

  def primary(): Expr =
    if matchTokens(FALSE) then LiteralExpr(Bool(false))
    else if matchTokens(TRUE) then LiteralExpr(Bool(true))
    else if matchTokens(NIL) then LiteralExpr(Nil)
    else if matchTokens(NUMBER) then LiteralExpr(Num(previous().literal.asInstanceOf[Double]))
    else if matchTokens(STRING) then LiteralExpr(Str(previous().literal.asInstanceOf[String]))
    else if matchTokens(IDENTIFIER) then VariableExpr(previous())
    else if (matchTokens(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      GroupingExpr(expr)
    }
    else throw error(peek(), "Expect expression.")

  def statement(): Stmt =
    if matchTokens(FOR) then forStatement()
    else if matchTokens(IF) then ifStatement()
    else if matchTokens(PRINT) then printStatement()
    else if matchTokens(RETURN) then returnStatement()
    else if matchTokens(WHILE) then whileStatement()
    else if matchTokens(LEFT_BRACE) then BlockStmt(block())
    else expressionStatement()

  def returnStatement(): Stmt =
    val keyword = previous()
    var value = if !check(SEMICOLON) then expression() else null
    consume(SEMICOLON, "Expect ';' after return value.")
    ReturnStmt(keyword, value)

  def forStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'for'.")
    var initializer = if matchTokens(SEMICOLON) then
      null
    else if matchTokens(VAR) then
      varDeclaration()
    else
      expressionStatement()

    var condition = if !check(SEMICOLON) then expression() else null
    consume(SEMICOLON, "Expect ';' after loop condition.")

    var increment = if !check(RIGHT_PAREN) then expression() else null
    consume(RIGHT_PAREN, "Expect ')' after for clauses.")

    var body = statement()
    if increment != null then
      body = BlockStmt(List(body, ExpressionStmt(increment)))

    if condition == null then condition = LiteralExpr(Bool(true))
    body = WhileStmt(condition, body)

    if initializer != null then
      body = BlockStmt(List(initializer, body))

    body

  def ifStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after if condition.")
    val thenBranch = statement()
    val elseBranch = if matchTokens(ELSE) then Some(statement()) else None
    IfStmt(condition, thenBranch, elseBranch)

  def printStatement(): Stmt =
    val value = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    PrintStmt(value)

  def expressionStatement(): Stmt =
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
    ExpressionStmt(expr)

  def whileStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after condition")
    val body = statement()
    WhileStmt(condition, body)

  def declaration(): Stmt =
    try
      if matchTokens(FUN) then function("function")
      else if matchTokens(VAR) then varDeclaration()
      else statement()
    catch
      case e : ParseError =>
        synchronize()
        null

  def function(kind: String): FunctionStmt =
    val name = consume(IDENTIFIER, s"Expect ${kind} name.")
    consume(LEFT_PAREN, s"Expect '(' after ${kind} name.")
    var parameters = mutable.ArrayBuffer.empty[Token]
    if !check(RIGHT_PAREN) then
      while
        if parameters.size >= MAX_ARGS then error(peek(), s"Can't have more than ${MAX_ARGS} parameters.")
        parameters.addOne(consume(IDENTIFIER, "Expect parameter name."))
        matchTokens(COMMA)
      do ()
    consume(RIGHT_PAREN, "Expect ')' after parameters.")
    consume(LEFT_BRACE, s"Expect '{' before ${kind} body.")
    val body = block()
    FunctionStmt(name, parameters.toList, body)

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
    var statements = mutable.ArrayBuffer.empty[Stmt]
    while !isAtEnd() do
      statements.addOne(declaration())
    statements.toList
