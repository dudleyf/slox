package lox

import scala.collection.mutable

class Environment(val enclosing: Option[Environment] = None):
  private val values = mutable.HashMap[String, Any]()

  def get(name: Token): Any = values.get(name.lexeme) match
    case Some(v) => v
    case None => enclosing match
      case Some(env) => env.get(name)
      case None => throw undefined(name)

  def define(name: String, value: Any): Unit =
    values.put(name, value)

  def assign(name: Token, value: Any): Unit =
    if values.contains(name.lexeme) then
      values.update(name.lexeme, value)
    else
      enclosing match
        case Some(env) => env.assign(name, value)
        case None => throw undefined(name)

  private def undefined(name: Token) =
    RuntimeError(name, s"Undefined variable '${name.lexeme}'.")


