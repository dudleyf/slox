package lox

import scala.collection.mutable

class Interpreter extends LoxEvaluator, ExprVisitor[Value], StmtVisitor[Unit] :
  import TokenType.*

  val globals = Environment()

  private val clockFn = new LoxCallable:
    override def arity(): Int = 0

    override def call(interpreter: LoxEvaluator, arguments: Seq[Value]): Value =
      Num(System.currentTimeMillis().toDouble)

    override def toString(): String = "<native fn>"

  globals.define("clock", clockFn)

  private var environment = globals

  def evaluate(expr: Expr): Value =
    expr.accept(this)

  def execute(stmt: Stmt): Unit =
    stmt.accept(this)

  def interpret(stmts: Seq[Stmt]): Unit =
    try
      for stmt <- stmts do execute(stmt)
    catch
      case e: RuntimeError => Lox.runtimeError(e)

  override def visit(stmt: PrintStmt): Unit =
    val value = evaluate(stmt.expression)
    println(value.toString)

  override def visit(stmt: ExpressionStmt): Unit =
    evaluate(stmt.expression)

  override def visit(expr: BinaryExpr): Value =
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    (left, expr.operator.tokenType, right) match
      case (Num(x), GREATER, Num(y)) => Bool(x > y)
      case (Num(x), GREATER_EQUAL, Num(y)) => Bool(x >= y)
      case (Num(x), LESS, Num(y)) => Bool(x < y)
      case (Num(x), LESS_EQUAL, Num(y)) => Bool(x <= y)
      case (l, BANG_EQUAL, r) => Bool(l != r)
      case (l, EQUAL_EQUAL, r) => Bool(l == r)
      case (Num(x), MINUS, Num(y)) => Num(x - y)
      case (Num(x), PLUS, Num(y)) => Num(x + y)
      case (Num(x), SLASH, Num(y)) => Num(x / y)
      case (Num(x), STAR, Num(y)) => Num(x * y)
      case (Str(x), PLUS, Str(y)) => Str(x + y)
      case _ => throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings.")

  override def visit(expr: GroupingExpr): Value =
    evaluate(expr.expression)

  override def visit(expr: LiteralExpr): Value =
    expr.value

  override def visit(expr: UnaryExpr): Value =
    (expr.operator.tokenType, evaluate(expr.right)) match
      case (MINUS, Num(x)) => Num(-x)
      case (MINUS, _) => throw RuntimeError(expr.operator, "Operand must be a number.")
      case (BANG, x) => Bool(!x.isTruthy)
      case _ => Nil

  override def visit(expr: VariableExpr): Value =
    environment.get(expr.name)

  override def visit(stmt: VarStmt): Unit =
    var value = if stmt.initializer != null then
      evaluate(stmt.initializer)
    else
      null
    environment.define(stmt.name.lexeme, value)

  override def visit(expr: AssignExpr): Value =
    val value = evaluate(expr.value)
    environment.assign(expr.name, value)
    value

  override def visit(stmt: BlockStmt): Unit =
    executeBlock(stmt.statements, Environment(Some(environment)))

  override def visit(stmt: IfStmt): Unit =
    if evaluate(stmt.condition).isTruthy then
      execute(stmt.thenBranch)
    else if stmt.elseBranch != null then
      execute(stmt.elseBranch)

  override def visit(expr: LogicalExpr): Value =
    val left = evaluate(expr.left)
    if expr.operator.tokenType == TokenType.OR then
      if left.isTruthy then return left
    else
      if !left.isTruthy then return left
    evaluate(expr.right)

  override def visit(stmt: WhileStmt): Unit =
    while evaluate(stmt.condition).isTruthy do
      execute(stmt.body)

  override def visit(expr: CallExpr): Value =
    val callee = evaluate(expr.callee)
    var arguments = mutable.ArrayBuffer.empty[Value]
    for argument <- expr.arguments do
      arguments.addOne(evaluate(argument))
    if !callee.isInstanceOf[LoxCallable] then
      throw new RuntimeError(expr.paren, "Can only call functions and classes.")
    val function = callee.asInstanceOf[LoxCallable]
    if arguments.size != function.arity() then
      throw new RuntimeError(expr.paren, s"Expected ${function.arity()} arguments but got ${arguments.size}.")
    function.call(this, arguments.toSeq)

  override def visit(stmt: FunctionStmt): Unit =
    val function = LoxFunction(stmt, environment)
    environment.define(stmt.name.lexeme, function)

  override def visit(stmt: ReturnStmt): Unit =
    var value = if stmt.value != null then evaluate(stmt.value) else null
    throw new Return(value)

  def executeBlock(statements: Seq[Stmt], environment: Environment): Unit =
    val previous = this.environment
    try
      this.environment = environment
      statements.foreach(execute)
    finally
      this.environment = previous
