package lox

sealed trait Value:
  def isTruthy: Boolean = this match
    case Nil => false
    case Bool(b) => b
    case _ => true

//  override def equals(obj: Any): Boolean = (this, obj) match
//    case (NilValue, NilValue) => true
//    case (NilValue, _) => false
//    case (a, b) => a equals b

case object Nil extends Value:
  override def toString: String = "nil"

case class Num(val value: Double) extends Value:
  override def toString: String =
    var text = value.toString()
    if text.endsWith(".0") then
      text.substring(0, text.length() - 2)
    else
      text

case class Bool(val value: Boolean) extends Value:
  override def toString: String = value.toString

case class Str(val value: String) extends Value:
  override def toString: String = value

trait LoxCallable extends Value:
  def arity(): Int
  def call(interpreter: LoxEvaluator, arguments: Seq[Value]): Value

  override def toString: String = "<callable>"

class LoxFunction(private val declaration: FunctionStmt,
                  private val closure: Environment) extends LoxCallable:
  override def arity(): Int = declaration.params.size

  override def call(interpreter: LoxEvaluator, arguments: Seq[Value]): Value =
    val environment = Environment(Some(closure))
    for ((param, arg) <- declaration.params.zip(arguments)) do
      environment.define(param.lexeme, arg)
    try
      interpreter.executeBlock(declaration.body, environment)
    catch
      case r: Return => return r.value
    null

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
