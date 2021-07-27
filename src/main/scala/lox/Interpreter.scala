package lox

class RuntimeError(val token: Token, msg: String) extends Exception(msg)

class Interpreter extends Visitor[Any] {

  import TokenType.*

  def evaluate(expr: Expr): Any =
    expr.accept(this)

  def isTruthy(obj: Any): Boolean = obj match {
    case null => false
    case b: Boolean => b
    case _ => true
  }

  def isEqual(a: Any, b: Any): Boolean = (a, b) match {
    case (null, null) => true
    case (null, _) => false
    case (a, b) => a.equals(b)
  }

  def num(obj: Any): Double = obj match {
    case d: Double => d
    case x => x.toString.toDouble
  }

  def checkNumberOperand(operator: Token, operand: Any): Unit = operand match {
    case o: Double => ()
    case _ => throw new RuntimeError(operator, "Operand must be a number")
  }

  def checkNumberOperands(operator: Token, left: Any, right: Any): Unit = (left, right) match {
    case (l: Double, r: Double) => ()
    case _ => throw new RuntimeError(operator, "Operands must be numbers")
  }

  def interpret(expression: Expr): Unit =
    try {
      val value = evaluate(expression)
      println(stringify(value))
    } catch {
      case e: RuntimeError => Lox.runtimeError(e)
    }

  def stringify(obj: Any): String = obj match {
    case null => "nil"
    case d: Double => {
      var text = d.toString()
      if (text.endsWith(".0")) {
        text = text.substring(0, text.length() - 2)
      }
      text
    }
    case _ => obj.toString()
  }

  override def visit(expr: Binary): Any =
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tokenType match {
      case GREATER => {
        checkNumberOperands(expr.operator, left, right)
        num(left) > num(right)
      }
      case GREATER_EQUAL => {
        checkNumberOperands(expr.operator, left, right)
        num(left) >= num(right)
      }
      case LESS => {
        checkNumberOperands(expr.operator, left, right)
        num(left) < num(right)
      }
      case LESS_EQUAL => {
        checkNumberOperands(expr.operator, left, right)
        num(left) <= num(right)
      }
      case BANG_EQUAL => !isEqual(left, right)
      case EQUAL_EQUAL => isEqual(left, right)
      case MINUS => {
        checkNumberOperands(expr.operator, left, right)
        num(left) - num(right)
      }
      case PLUS => (left, right) match {
        case (x: Double, y: Double) => num(left) + num(right)
        case (x: String, y: String) => x + y
        case _ => throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings.")
      }
      case SLASH => {
        checkNumberOperands(expr.operator, left, right)
        num(left) / num(right)
      }
      case STAR => {
        checkNumberOperands(expr.operator, left, right)
        num(left) * num(right)
      }
      case _ => null
    }

  override def visit(expr: Grouping): Any =
    evaluate(expr.expression)

  override def visit(expr: Literal): Any =
    expr.value

  override def visit(expr: Unary): Any =
    val right = evaluate(expr.right)
    expr.operator.tokenType match {
      case MINUS => {
        checkNumberOperand(expr.operator, right)
        -num(right)
      }
      case BANG => !isTruthy(right)
      case _ => null
    }


}
