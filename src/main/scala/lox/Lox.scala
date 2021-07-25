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

  def run(source: String) =
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()
    for token <- tokens do println(token)


object Lox:
  var hadError = false;

  def error(line: Int, message: String) =
    report(line, "", message)

  private def report(line: Int, where: String, message: String) =
    System.err.println(s"[line ${line}] Error${where}: ${message}")
  hadError = true
