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
import analyzer._
import code._


object Main {

  private def processOptions(args: Array[String]): (Context, Boolean, Boolean, Boolean, Boolean) = {

    val reporter = new Reporter()
    var outDir: Option[File] = None
    var files: List[File] = Nil
    var printTokens = false
    var printPretty = false
    var printAST = false
    var printSYMID = false

    def processOption(args: List[String]): Unit = args match {
      case "-d" :: out :: args =>
        outDir = Some(new File(out))
        processOption(args)

      case "--tokens" :: args =>
        printTokens = true
        processOption(args)

      case "--format" :: args =>
        printPretty = true
        processOption(args)

      case "--ast" :: args =>
        printAST = true
        processOption(args)

      case "--symid" :: args =>
        printSYMID = true
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

    (Context(reporter = reporter, file = Some(files.head), outDir = outDir), printTokens, printPretty, printAST, printSYMID)
  }

  def main(args: Array[String]) {
    val (ctx, printTokens, printPretty, printAST, printSYMID) = processOptions(args)

    if(printTokens) {
      for(t <- (Lexer andThen PrintTokens).run(ctx)(ctx.file.get)) {
        println()
      }
    } else if(printAST) {
      val pipeline = Lexer andThen Parser

      if(printSYMID){
        println(
          (pipeline andThen ClassTemplateExpander andThen NameAnalysis).run(ctx)(ctx.file.get)
            map PrintSYMID getOrElse "Invalid input program."
        )
      } else {
        println(
          pipeline.run(ctx)(ctx.file.get)
            map PrintAST getOrElse "Failed to parse input."
        )
      }
    } else if(printPretty) {
      val pipeline = Lexer andThen Parser

      print(
        (if(printSYMID) {
          pipeline andThen ClassTemplateExpander andThen NameAnalysis
        } else {
          pipeline
        }).run(ctx)(ctx.file.get)
          map Printer(printSYMID) getOrElse "Compilation failed.\n"
      )
    } else {
      val pipeline = Lexer andThen Parser andThen ClassTemplateExpander andThen NameAnalysis andThen TypeChecking andThen CodeGeneration
      pipeline.run(ctx)(ctx.file.get)
      if(ctx.reporter.hasErrors) {
        System.exit(1)
      }
    }

  }
}
