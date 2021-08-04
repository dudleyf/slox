package lox

trait LoxCallable {
  def arity(): Int
  def call(interpreter: Interpreter, arguments: List[Any]): Any
}
