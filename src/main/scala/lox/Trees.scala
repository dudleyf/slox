package lox

sealed abstract class Expr:
  def accept[T](visitor: ExprVisitor[T]): T

trait ExprVisitor[T]:
  def visit(expr: BinaryExpr): T
  def visit(expr: GroupingExpr): T
  def visit(expr: LiteralExpr): T
  def visit(expr: UnaryExpr): T
  def visit(expr: VariableExpr): T
  def visit(expr: AssignExpr): T
  def visit(expr: LogicalExpr): T

case class BinaryExpr(left: Expr, operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)

case class GroupingExpr(expression: Expr) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)

case class LiteralExpr(value: Any) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this);

case class UnaryExpr(operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)

case class VariableExpr(name: Token) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)

case class AssignExpr(name: Token, value: Expr) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)

case class LogicalExpr(left: Expr, operator: Token, right: Expr) extends Expr:
  override def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)

sealed abstract class Stmt:
  def accept[R](visitor: StmtVisitor[R]): R

trait StmtVisitor[R]:
  def visit(stmt: PrintStmt): R
  def visit(stmt: ExpressionStmt): R
  def visit(stmt: VarStmt): R
  def visit(stmt: BlockStmt): R
  def visit(stmt: IfStmt): R
  def visit(stmt: WhileStmt): R

case class PrintStmt(expression: Expr) extends Stmt:
  override def accept[T](visitor: StmtVisitor[T]): T = visitor.visit(this)

case class ExpressionStmt(expression: Expr) extends Stmt:
  override def accept[T](visitor: StmtVisitor[T]): T = visitor.visit(this)

case class VarStmt(name: Token, initializer: Expr) extends Stmt:
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visit(this)

case class BlockStmt(statements: List[Stmt]) extends Stmt:
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visit(this)

case class IfStmt(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt:
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visit(this)

case class WhileStmt(condition: Expr, body: Stmt) extends Stmt:
  override def accept[R](visitor: StmtVisitor[R]): R = visitor.visit(this)
