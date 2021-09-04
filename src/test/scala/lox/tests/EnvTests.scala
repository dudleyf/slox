package lox.tests

import lox.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.FixtureAnyFunSuite
import org.scalatest.matchers.should.Matchers

class EnvTests extends AnyFlatSpec with Matchers :
  trait Envs {
    val x = Token(TokenType.IDENTIFIER, "x")
    val y = Token(TokenType.IDENTIFIER, "y")
    val z = Token(TokenType.IDENTIFIER, "z")

    val grandparent = Env()
    val parent = Env(Some(grandparent))
    val child = Env(Some(parent))
  }

  "get" should "define and get variables" in new Envs {
    val env = Env()
    env.define(x, Num(1234))
    env.get(x) shouldEqual Num(1234)
  }

  "get" should "search up the parent chain" in new Envs {
    grandparent.define(z, Str("baz"))
    parent.define(x, Str("foo"))
    child.define(y, Str("bar"))

    child.get(y) shouldEqual Str("bar")
    child.get(x) shouldEqual Str("foo")
    child.get(z) shouldEqual Str("baz")
  }

  "assign" should "set an existing var" in new Envs {
    val env = Env()
    env.define(x, Str("foo"))
    env.assign(x, Str("bar"))
    env.get(x) shouldEqual Str("bar")
  }

  "assign" should "throw an error on a nonexistant var" in new Envs {
    val env = Env()
    an [RuntimeError] should be thrownBy env.assign(x, Str("foo"))
  }

  "assign" should "set a var defined in a parent environment" in new Envs {
    grandparent.define(z, Str("baz"))
    parent.define(x, Str("foo"))
    child.define(y, Str("bar"))

    child.assign(z, Str("quux"))

    child.get(z) shouldEqual Str("quux")
  }

  "ancestor" should "return the parent environment at the given index" in new Envs {
    child.ancestor(0) shouldEqual Some(child)
    child.ancestor(1) shouldEqual Some(parent)
    child.ancestor(2) shouldEqual Some(grandparent)
  }

  "getAt" should "return the value of a var in a given parent environment" in new Envs {
    grandparent.define(x, Str("baz"))
    parent.define(x, Str("foo"))
    child.define(x, Str("bar"))

    child.getAt(0, x.lexeme) shouldEqual Str("bar")
    child.getAt(1, x.lexeme) shouldEqual Str("foo")
    child.getAt(2, x.lexeme) shouldEqual Str("baz")
  }

  "assignAt" should "return the value of a var in a given parent environment" in new Envs {
    grandparent.define(x, Str("baz"))
    parent.define(x, Str("foo"))
    child.define(x, Str("bar"))

    child.assignAt(0, x, Str("child"))
    child.assignAt(1, x, Str("parent"))
    child.assignAt(2, x, Str("grandparent"))

    child.getAt(0, x.lexeme) shouldEqual Str("child")
    child.getAt(1, x.lexeme) shouldEqual Str("parent")
    child.getAt(2, x.lexeme) shouldEqual Str("grandparent")
  }
