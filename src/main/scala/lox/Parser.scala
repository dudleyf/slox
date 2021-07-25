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


class AstPrinter extends Visitor[String]:
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

class Parser:
  ???
