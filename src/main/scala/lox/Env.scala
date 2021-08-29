package lox

import scala.collection.mutable

class Env(val enclosing: Option[Env] = None):
  private val values = mutable.Map[String, Value]()

  def get(name: Token): Value = values.get(name.lexeme) match
    case Some(v) => v
    case None => enclosing match
      case Some(env) => env.get(name)
      case None => throw undefined(name)

  def getAt(distance: Int, name: String): Value = ancestor(distance) match
    case None => ???
    case Some(e) => e.values(name)

  def define(name: String, value: Value): Unit =
    values.put(name, value)

  def define(name: Token, value: Value): Unit =
    define(name.lexeme, value)

  def assign(name: Token, value: Value): Unit =
    if values.contains(name.lexeme) then
      values.update(name.lexeme, value)
    else
      enclosing match
        case Some(env) => env.assign(name, value)
        case None => throw undefined(name)

  def assignAt(distance: Int, name: Token, value: Value): Unit = ancestor(distance) match
    case None => ???
    case Some(e) => e.values(name.lexeme) = value

  def ancestor(distance: Int): Option[Env] =
    if distance == 0 then Some(this)
    else enclosing.flatMap(e => e.ancestor(distance - 1))

  private def undefined(name: Token) =
    RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
