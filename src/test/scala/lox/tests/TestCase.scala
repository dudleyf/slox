package lox.tests

import lox.*
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.*

import java.io.ByteArrayOutputStream
import scala.Console.in
import scala.collection.mutable.ListBuffer

abstract class TestCase extends AnyFlatSpec with should.Matchers :
  def captureStdOut[T](thunk: => T): String =
    val stdOut = ByteArrayOutputStream()
    Console.withOut(stdOut)(thunk)
    stdOut.toString()

  def run(source: String) =
    captureStdOut { Lox().run(source) }
