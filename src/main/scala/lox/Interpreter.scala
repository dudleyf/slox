package lox

import scala.collection.mutable

class RuntimeError(val token: Token, msg: String) extends Exception(msg)

class Interpreter extends ExprVisitor[Any], StmtVisitor[Unit] :
  import TokenType.*

  val globals = Environment()

  globals.define("clock", new LoxCallable {
    override def arity(): Int = 0

    override def call(interpreter: Interpreter, arguments: List[Any]): Any =
      System.currentTimeMillis().toDouble

    override def toString(): String = "<native fn>"
  })

  private var environment = globals

  def isTruthy(obj: Any): Boolean = obj match {
    case null => false
    case b: Boolean => b
    case _ => true
  }

  def isEqual(a: Any, b: Any): Boolean = (a, b) match
    case (null, null) => true
    case (null, _) => false
    case (a, b) => a.equals(b)

  def num(obj: Any): Double = obj match
    case d: Double => d
    case x => x.toString.toDouble

  def checkNumberOperand(operator: Token, operand: Any): Unit = operand match
    case o: Double => ()
    case _ => throw new RuntimeError(operator, "Operand must be a number")

  def checkNumberOperands(operator: Token, left: Any, right: Any): Unit = (left, right) match
    case (l: Double, r: Double) => ()
    case _ => throw new RuntimeError(operator, "Operands must be numbers")

  def evaluate(expr: Expr): Any =
    expr.accept(this)

  def execute(stmt: Stmt): Unit =
    stmt.accept(this)

  def interpret(stmts: List[Stmt]): Unit =
    try
      for stmt <- stmts do execute(stmt)
    catch
      case e: RuntimeError => Lox.runtimeError(e)

  def stringify(obj: Any): String = obj match
    case null => "nil"
    case d: Double => {
      var text = d.toString()
      if text.endsWith(".0") then
        text = text.substring(0, text.length() - 2)
      text
    }
    case _ => obj.toString()

  override def visit(stmt: PrintStmt): Unit =
    val value = evaluate(stmt.expression)
    println(stringify(value))

  override def visit(stmt: ExpressionStmt): Unit =
    evaluate(stmt.expression)

  override def visit(expr: BinaryExpr): Any =
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tokenType match
      case GREATER =>
        checkNumberOperands(expr.operator, left, right)
        num(left) > num(right)
      case GREATER_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        num(left) >= num(right)
      case LESS =>
        checkNumberOperands(expr.operator, left, right)
        num(left) < num(right)
      case LESS_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        num(left) <= num(right)
      case BANG_EQUAL =>
        !isEqual(left, right)
      case EQUAL_EQUAL =>
        isEqual(left, right)
      case MINUS =>
        checkNumberOperands(expr.operator, left, right)
        num(left) - num(right)
      case PLUS => (left, right) match
        case (x: Double, y: Double) => num(left) + num(right)
        case (x: String, y: String) => x + y
        case _ => throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings.")
      case SLASH =>
        checkNumberOperands(expr.operator, left, right)
        num(left) / num(right)
      case STAR =>
        checkNumberOperands(expr.operator, left, right)
        num(left) * num(right)
      case _ => null

  override def visit(expr: GroupingExpr): Any =
    evaluate(expr.expression)

  override def visit(expr: LiteralExpr): Any =
    expr.value

  override def visit(expr: UnaryExpr): Any =
    val right = evaluate(expr.right)
    expr.operator.tokenType match
      case MINUS =>
        checkNumberOperand(expr.operator, right)
        -num(right)
      case BANG => !isTruthy(right)
      case _ => null

  override def visit(expr: VariableExpr): Any =
    environment.get(expr.name)

  override def visit(stmt: VarStmt): Unit =
    var value = if stmt.initializer != null then
      evaluate(stmt.initializer)
    else
      null

    environment.define(stmt.name.lexeme, value)

  override def visit(expr: AssignExpr): Any =
    val value = evaluate(expr.value)
    environment.assign(expr.name, value)
    value

  override def visit(stmt: BlockStmt): Unit =
    executeBlock(stmt.statements, Environment(environment))

  override def visit(stmt: IfStmt): Unit =
    if isTruthy(evaluate(stmt.condition)) then
      execute(stmt.thenBranch)
    else if stmt.elseBranch != null then
      execute(stmt.elseBranch)

  override def visit(expr: LogicalExpr): Any =
    val left = evaluate(expr.left)
    if expr.operator.tokenType == TokenType.OR then
      if isTruthy(left) then return left
      else if !isTruthy(left) then return left
    evaluate(expr.right)

  override def visit(stmt: WhileStmt): Unit =
    while isTruthy(evaluate(stmt.condition)) do
      execute(stmt.body)

  override def visit(expr: CallExpr): Any =
    val callee = evaluate(expr.callee)
    var arguments = mutable.ArrayBuffer.empty[Any]
    for argument <- expr.arguments do
      arguments.addOne(evaluate(argument))
    if !callee.isInstanceOf[LoxCallable] then
      throw new RuntimeError(expr.paren, "Can only call functions and classes.")
    val function = callee.asInstanceOf[LoxCallable]
    if arguments.size != function.arity() then
      throw new RuntimeError(expr.paren, s"Expected ${function.arity()} arguments but got ${arguments.size}.")

    function.call(this, arguments.toList)

  def executeBlock(statements: List[Stmt], environment: Environment): Unit =
    val previous = this.environment
    try
      this.environment = environment
      statements.foreach(execute)
    finally
      this.environment = previous
