package lox

import scala.collection.{GenMap, mutable}

object ClockFn extends LoxCallable:
  override def arity(): Int = 0

  override def call(interpreter: Interpreter, arguments: Seq[Value]): Value =
    Num(System.currentTimeMillis().toDouble)

  override def toString(): String = "<native fn>"

class Interpreter:
  import TokenType.*

  val globals = Environment()
  globals.define("clock", ClockFn)

  private var environment = globals

  def execute(stmts: Seq[Stmt]): Unit =
    try
      for stmt <- stmts do execute(stmt)
    catch
      case e: RuntimeError => Lox.runtimeError(e)

  def execute(stmt: Stmt): Unit = stmt match
    case PrintStmt(expr) => println(evaluate(expr).toString)
    case ExpressionStmt(expr) => evaluate(expr)
    case VarStmt(token, init) =>
      val value = if init != null then evaluate(init) else null
      environment.define(token.lexeme, value)
    case BlockStmt(stmts) => executeBlock(stmts, Environment(Some(environment)))
    case IfStmt(cond, thenBranch, elseBranch) =>
      if evaluate(cond).isTruthy then
        execute(thenBranch)
      else if elseBranch != null then
        execute(elseBranch)
    case WhileStmt(cond, body) => while evaluate(cond).isTruthy do execute(body)
    case f @ FunctionStmt(token, _, _) =>
      environment.define(token.lexeme, LoxFunction(f, environment))
    case ReturnStmt(_, value) => throw Return(evaluate(value))


  def evaluate(expr: Expr): Value = expr match
    case BinaryExpr(left, op, right) => (evaluate(left), op.tokenType, evaluate(right)) match
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
      case _ => throw new RuntimeError(op, "Operands must be two numbers or two strings.")
    case GroupingExpr(expr) => evaluate(expr)
    case LiteralExpr(value) => value
    case UnaryExpr(op, right) => (op.tokenType, evaluate(right)) match
      case (MINUS, Num(x)) => Num(-x)
      case (MINUS, _) => throw RuntimeError(op, "Operand must be a number.")
      case (BANG, x) => Bool(!x.isTruthy)
      case _ => Nil
    case VariableExpr(token) => environment.get(token)
    case AssignExpr(token, valueExpr) =>
      val value = evaluate(valueExpr)
      environment.assign(token, value)
      value
    case LogicalExpr(leftExpr, op, rightExpr) => (evaluate(leftExpr), op.tokenType, rightExpr) match
      case (left, OR, right) =>
        if left.isTruthy then left else evaluate(right)
      case (left, AND, right) =>
        if !left.isTruthy then left else evaluate(right)
    case CallExpr(calleeExpr, paren, args) =>
      val callee = evaluate(calleeExpr)
      var argVals = args.map(evaluate)
      callee match
        case fn: LoxCallable =>
          if argVals.size != fn.arity() then
            throw new RuntimeError(paren, s"Expected ${fn.arity()} arguments but got ${args.size}.")
          fn.call(this, argVals.toSeq)
        case _ => throw new RuntimeError(paren, "Can only call functions and classes.")

  def executeBlock(statements: Seq[Stmt], environment: Environment): Unit =
    val previous = this.environment
    try
      this.environment = environment
      statements.foreach(execute)
    finally
      this.environment = previous
