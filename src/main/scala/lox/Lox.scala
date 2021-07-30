package lox

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.io.StdIn.readLine

object Lox {
  val ERROR_EXIT = 65
  val RUNTIME_ERROR_EXIT = 70

  var hadError = false
  var hadRuntimeError = false

  val interpreter = Interpreter()

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

  def runFile(path: String) =
    var source = Files.readString(Path.of(path), UTF_8)
    run(source)
    if hadError then System.exit(ERROR_EXIT)
    if hadRuntimeError then System.exit(RUNTIME_ERROR_EXIT)

  def runPrompt() =
    var line = ""
    while (line != null) {
      line = readLine("> ");
      run(line)
      hadError = false
    }

  def run(source: String): Unit =
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()
    val parser = Parser(tokens)
    val stmts = parser.parse()
    if hadError then () else interpreter.interpret(stmts)
}
