package lox

sealed trait Tree

sealed abstract class Expr extends Tree

case class BinaryExpr(left: Expr, operator: Token, right: Expr) extends Expr
case class GroupingExpr(expression: Expr) extends Expr
case class LiteralExpr(value: Value) extends Expr
case class UnaryExpr(operator: Token, right: Expr) extends Expr
case class VariableExpr(name: Token) extends Expr
case class AssignExpr(name: Token, value: Expr) extends Expr
case class LogicalExpr(left: Expr, operator: Token, right: Expr) extends Expr
case class CallExpr(callee: Expr, paren: Token, arguments: Seq[Expr]) extends Expr

sealed abstract class Stmt extends Tree

case class PrintStmt(expression: Expr) extends Stmt
case class ExpressionStmt(expression: Expr) extends Stmt
case class VarStmt(name: Token, initializer: Option[Expr]) extends Stmt
case class BlockStmt(statements: Seq[Stmt]) extends Stmt
case class IfStmt(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]) extends Stmt
case class WhileStmt(condition: Expr, body: Stmt) extends Stmt
case class FunctionStmt(name: Token, params: Seq[Token], body: Seq[Stmt]) extends Stmt
case class ReturnStmt(keyword: Token, value: Expr) extends Stmt
