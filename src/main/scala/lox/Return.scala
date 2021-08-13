package lox

class Return(val value: Value) extends RuntimeException(null, null, false, false)
