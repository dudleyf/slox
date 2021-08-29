package lox

import scala.collection.mutable

class Resolver(val interpreter: Interpreter):
  private enum LoxFunctionType:
    case None, Function

  val scopes = mutable.Stack.empty[mutable.Map[String, Boolean]]
  private var currentFunctionType = LoxFunctionType.None

  def resolve(stmts: Seq[Stmt]): Unit =
    stmts.foreach(resolve)

  def resolve(stmt: Stmt): Unit = stmt match
    case BlockStmt(stmts) =>
      beginScope()
      resolve(stmts)
      endScope()
    case VarStmt(name, init) =>
      declare(name)
      init.foreach(resolve)
      define(name)
    case f @ FunctionStmt(name, _, _) =>
      declare(name)
      define(name)
      resolveFunction(f, LoxFunctionType.Function)
    case ExpressionStmt(expr) => resolve(expr)
    case IfStmt(condition, thenBranch, elseBranch) =>
      resolve(condition)
      resolve(thenBranch)
      elseBranch.foreach(resolve)
    case PrintStmt(expr) => resolve(expr)
    case ReturnStmt(keyword, expr) =>
      currentFunctionType match
        case LoxFunctionType.None => Lox.error(keyword, "Can't return from top-level code.")
        case _ => ()
      resolve(expr)
    case WhileStmt(condition, body) =>
      resolve(condition)
      resolve(body)

  def resolve(expr: Expr): Unit = expr match
    case VariableExpr(name) =>
      if scopes.nonEmpty then scopes.top.get(name.lexeme) match
        case Some(false) => Lox.error(name, "Can't read local variable in its own initializer.")
        case _ => ()
      resolveLocal(expr.asInstanceOf[Expr], name)
    case AssignExpr(name, right) =>
      resolve(right)
      resolveLocal(expr, name)
    case BinaryExpr(left, _, right) =>
      resolve(left)
      resolve(right)
    case LogicalExpr(left, _, right) =>
      resolve(left)
      resolve(right)
    case CallExpr(callee, _, args) =>
      resolve(callee)
      args.foreach(resolve)
    case GroupingExpr(expr) => resolve(expr)
    case LiteralExpr(_) => ()
    case UnaryExpr(_, right) => resolve(right)

  private def beginScope(): Unit =
    scopes.push(mutable.Map.empty)

  private def endScope(): Unit =
    scopes.pop()

  private def declare(name: Token): Unit =
    if scopes.isEmpty then
      ()
    else if scopes.top.contains(name.lexeme) then
      Lox.error(name, "Variable with this name already declared in this scope.")
    else
      scopes.top(name.lexeme) = false

  private def define(name: Token): Unit =
    if scopes.isEmpty then ()
    else scopes.top(name.lexeme) = true

//  private def resolveLocal(expr: Expr, name: Token): Unit =
//    for (scope, i) <- scopes.zipWithIndex do
//      if scope.contains(name.lexeme) then interpreter.resolve(expr, i)

  private def resolveLocal(expr: Expr, name: Token): Unit = {
    for (i <- scopes.size - 1 to 0 by -1) {
      if (scopes(i).contains(name.lexeme)) {
        interpreter.resolve(expr, scopes.size - 1 - i)
        return
      }
    }
  }

  private def resolveFunction(fn: FunctionStmt, fnType: LoxFunctionType): Unit =
    val enclosingFunctionType = currentFunctionType
    currentFunctionType = fnType
    beginScope()
    for param <- fn.params do
      declare(param)
      define(param)
    resolve(fn.body)
    endScope()
    currentFunctionType = enclosingFunctionType
