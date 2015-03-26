/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc

import utils._
import java.io.File

import lexer._
import ast._

object Main {

  private def processOptions(args: Array[String]): (Context, Boolean, Boolean) = {

    val reporter = new Reporter()
    var outDir: Option[File] = None
    var files: List[File] = Nil
    var printTokens = false
    var printAST = false

    def processOption(args: List[String]): Unit = args match {
      case "-d" :: out :: args =>
        outDir = Some(new File(out))
        processOption(args)

      case "--tokens" :: args =>
        printTokens = true
        processOption(args)

      case "--ast" :: args =>
        printAST = true
        processOption(args)

      case f ::args =>
        files = new File(f) :: files
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, " + files.size + " file(s) given.")
    }

    (Context(reporter = reporter, file = Some(files.head), outDir = outDir), printTokens, printAST)
  }

  def main(args: Array[String]) {
    val (ctx, printTokens, printAST) = processOptions(args)

    if(printTokens) {
      for(t <- (Lexer andThen PrintTokens).run(ctx)(ctx.file.get)) {
        println()
      }
    } else if(printAST){
      val pipeline = Lexer andThen Parser

      println(
        pipeline.run(ctx)(ctx.file.get)
          map PrintAST getOrElse "Failed to parse input."
      )

    } else {
      val pipeline = Lexer andThen Parser

      print(
        pipeline.run(ctx)(ctx.file.get)
          map Printer getOrElse "Failed to parse input.\n"
      )
    }

  }
}
