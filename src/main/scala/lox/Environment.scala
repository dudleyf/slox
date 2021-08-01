package lox

import scala.collection.mutable

class Environment:
  private val values = mutable.HashMap[String, Any]()

  def get(name: Token): Any =
    values.get(name.lexeme) match
      case Some(v) => v
      case None =>
        throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")


  def define(name: String, value: Any): Unit =
    values.put(name, value)
