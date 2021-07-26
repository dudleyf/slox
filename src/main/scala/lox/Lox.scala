package lox

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.io.StdIn.readLine

class Lox:
  import Lox._

  def runFile(path: String) =
    var source = Files.readString(Path.of(path), UTF_8)
    run(source)
    if hadError then System.exit(65)

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
    val expr = parser.parse()
    if hadError then return
    println(AstPrinter.print(expr))



object Lox:
  var hadError = false;

  def error(line: Int, message: String) =
    report(line, "", message)

  def error(token: Token, message: String) =
    if (token.tokType == TokenType.EOF) {
      report(token.line, " at end", message)
    } else {
      report(token.line, " at '" + token.lexeme + "'", message)
    }

  private def report(line: Int, where: String, message: String) =
    System.err.println(s"[line ${line}] Error${where}: ${message}")
    hadError = true
