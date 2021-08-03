package lox

import lox.*
import lox.TestLox.{eval, parse, scan}
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*

import java.io.ByteArrayOutputStream
import scala.Console.in
import scala.collection.mutable.ListBuffer

object TestLox {
  def scan(source: String): List[Token] =
    val scanner = Scanner(source)
    scanner.scanTokens()

  def parse(source: String): List[Stmt] =
    val tokens = scan(source)
    val parser = Parser(tokens)
    parser.parse()

  def eval(source: String): String =
    val stmts = parse(source)
    val interpreter = Interpreter()
    val stdOut = ByteArrayOutputStream()
    Console.withOut(stdOut) {
      interpreter.interpret(stmts)
    }
    stdOut.toString()
}

class TestLox:
  val interpreter = Interpreter()
  val outputBuffer = StringBuilder()

  import TestLox.*

  def execute(source: String): String =
    val stmts = parse(source)
    val stdOut = ByteArrayOutputStream()
    Console.withOut(stdOut) {
      interpreter.interpret(stmts)
    }
    val result = stdOut.toString()
    outputBuffer ++= result
    result

abstract class TestCase extends AnyFunSuite with should.Matchers:
  import TestLox.*
