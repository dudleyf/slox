package lox

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Scanner
import scala.io.StdIn.readLine


def main(args: Array[String]): Unit =
  if (args.length > 1) {
    println("Usage: slox [script]")
    System.exit(64);
  }

  val lox = Lox()
  if (args.length == 1) then lox.runFile(args(0)) else lox.runPrompt()
