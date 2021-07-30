package lox

sealed abstract class Expr:
  def accept[T](visitor: ExprVisitor[T]): T

trait ExprVisitor[T]:
  def visit(expr: BinaryExpr): T
  def visit(expr: GroupingExpr): T
  def visit(expr: LiteralExpr): T
  def visit(expr: UnaryExpr): T

case class BinaryExpr(left: Expr, operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)

case class GroupingExpr(expression: Expr) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)

case class LiteralExpr(value: Any) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this);

case class UnaryExpr(operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)


object TreePrinter extends ExprVisitor[String], StmtVisitor[String]:
  override def visit(expr: BinaryExpr): String =
    parenthesize(expr.operator.lexeme, expr.left, expr.right)

  override def visit(expr: GroupingExpr): String =
    parenthesize("group", expr.expression)

  override def visit(expr: LiteralExpr): String =
    if expr.value == null then "nil" else expr.value.toString()

  override def visit(expr: UnaryExpr): String =
    parenthesize(expr.operator.lexeme, expr.right)

  override def visit(stmt: PrintStmt): String =
    parenthesize("print", stmt.expression)

  override def visit(stmt: ExpressionStmt): String =
    parenthesize("expr", stmt.expression)

  def print(expr: Expr): String = expr.accept(this)

  def print(stmt: Stmt): String = stmt.accept(this)

  def print(stmts: List[Stmt]): String = stmts.map(print).mkString("\n")

  def parenthesize(str: String, exprs: Expr*): String =
    var sb = StringBuilder()
    sb ++= "("
    sb ++= str
    sb ++= " "
    sb ++= exprs.map((x) => x.accept(this)).mkString(" ")
    sb ++= ")"
    sb.toString()


sealed abstract class Stmt:
  def accept[R](visitor: StmtVisitor[R]): R

trait StmtVisitor[R]:
  def visit(stmt: PrintStmt): R
  def visit(stmt: ExpressionStmt): R

case class PrintStmt(expression: Expr) extends Stmt:
  override def accept[T](visitor: StmtVisitor[T]): T = visitor.visit(this)

case class ExpressionStmt(expression: Expr) extends Stmt:
  override def accept[T](visitor: StmtVisitor[T]): T = visitor.visit(this)
