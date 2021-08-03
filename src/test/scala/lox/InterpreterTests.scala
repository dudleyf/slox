package lox

import lox.Lox.interpreter

class InterpreterTests extends TestCase :
  import TestLox.*

  test("evaluates a simple arithmetic expression") {
    eval("print 1+2;") shouldEqual ("3\n")
  }

  test("variable assignment") {
    eval("var x = 1;\nprint x;") shouldEqual("1\n")
  }
