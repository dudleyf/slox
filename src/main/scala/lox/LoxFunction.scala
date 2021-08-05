package lox

class LoxFunction(private val declaration: FunctionStmt) extends LoxCallable:
  override def arity(): Int = declaration.params.size

  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val environment = Environment(interpreter.globals)
    for ((param, arg) <- declaration.params.zip(arguments)) do
      environment.define(param.lexeme, arg)
    interpreter.executeBlock(declaration.body, environment)
    null

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
