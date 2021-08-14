package lox

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.io.StdIn.readLine

object Lox:
  val ERROR_EXIT = 65
  val RUNTIME_ERROR_EXIT = 70

  var hadError = false
  var hadRuntimeError = false

  def error(line: Int, message: String) =
    report(line, "", message)

  def error(token: Token, message: String) =
    if (token.tokenType == TokenType.EOF) {
      report(token.line, " at end", message)
    } else {
      report(token.line, " at '" + token.lexeme + "'", message)
    }

  def runtimeError(error: RuntimeError): Unit =
    System.err.println(error.getMessage + "\n[line " + error.token.line + "]")
    hadRuntimeError = true

  private def report(line: Int, where: String, message: String) =
    System.err.println(s"[line ${line}] Error${where}: ${message}")
    hadError = true

class Lox:
  import Lox._

  val interpreter = Interpreter()

  def scan(source: String): List[Token] =
    val scanner = Scanner(source)
    scanner.scanTokens()

  def parse(source: String): Seq[Stmt] =
    val tokens = scan(source)
    val parser = Parser(tokens)
    parser.parse()

  def evaluate(expr: Expr): Value =
    interpreter.evaluate(expr)

  def execute(stmt: Stmt): Unit =
    interpreter.execute(stmt)

  def execute(stmts: Seq[Stmt]): Unit =
    try
      for stmt <- stmts do execute(stmt)
    catch
      case e: RuntimeError => Lox.runtimeError(e)

  def executeBlock(statements: Seq[Stmt], environment: Environment): Unit =
    interpreter.executeBlock(statements, environment)

  def run(source: String): Unit =
    val stmts = parse(source)
    if hadError then () else execute(stmts)

  def runPrompt() =
    var line = ""
    while (line != null) {
      line = readLine("> ");
      run(line)
      hadError = false
    }

  def runFile(path: String) =
    var source = Files.readString(Path.of(path), UTF_8)
    run(source)
    if hadError then System.exit(ERROR_EXIT)
    if hadRuntimeError then System.exit(RUNTIME_ERROR_EXIT)
