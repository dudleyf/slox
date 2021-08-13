package lox

trait LoxEvaluator:
  def evaluate(expr: Expr): Value

  def execute(stmt: Stmt): Unit

  def interpret(stmts: Seq[Stmt]): Unit

  def executeBlock(statements: Seq[Stmt], environment: Environment): Unit
