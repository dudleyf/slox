package lox

class LoxFunction(private val declaration: FunctionStmt,
                  private val closure: Environment) extends LoxCallable:
  override def arity(): Int = declaration.params.size

  override def call(interpreter: Interpreter, arguments: List[Any]): Any =
    val environment = Environment(closure)
    for ((param, arg) <- declaration.params.zip(arguments)) do
      environment.define(param.lexeme, arg)
    try
      interpreter.executeBlock(declaration.body, environment)
    catch
      case r: Return => return r.value
    null

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
