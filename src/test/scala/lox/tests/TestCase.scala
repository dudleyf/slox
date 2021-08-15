package lox.tests

import lox.*
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*

import java.io.ByteArrayOutputStream
import scala.Console.in
import scala.collection.mutable.ListBuffer

trait TestHelpers {
  def scan(source: String): Seq[Token] = Lox().scan(source)

  def parse(source: String): Seq[Stmt] = Lox().parse(source)

  def execute(source: String): String =
    val interpreter = Lox()
    val stdOut = ByteArrayOutputStream()
    Console.withOut(stdOut) {
      interpreter.run(source)
    }
    stdOut.toString()
}

abstract class TestCase extends AnyFunSuite with should.Matchers with TestHelpers
