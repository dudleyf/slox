package lox

import lox.TestLox.eval

class InterpreterTests extends TestCase :
  test("evaluates a simple arithmetic expression") {
    eval("print 1+2;") shouldEqual ("3")
  }
