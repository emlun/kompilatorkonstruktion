package koolc
package utils

import java.io.File
import scala.io.Source
import scala.util.Try

import org.scalatest.Matchers

import lexer._
import ast.Trees._

trait TestUtils extends Matchers with ReporterMatchers {

  def pipeline: Pipeline[Iterator[Token], Option[Program]]

  def checkResult(body: (Context, Option[Program]) => Unit) = new Pipeline[Option[Program], Unit] {
    def run(ctx: Context)(program: Option[Program]) = body(ctx, program)
  }

  def checkResultForFile(path: String)(body: (Context, Option[Program]) => Unit) = {
    val input = Try(new File(getClass.getResource(path).toURI())) getOrElse fail("File " + path + " not found.")
    (Lexer andThen pipeline andThen checkResult(body)).run(Context(new Reporter, None, Some(input)))(input)
  }
  def checkResultForString(source: String)(body: (Context, Option[Program]) => Unit) = {
    (SourceLexer andThen pipeline andThen checkResult(body)).run(Context(new Reporter, None, None))(Source fromString source)
  }

  def assertFileSucceeds(path: String) = checkResultForFile(path) { (ctx, program) =>
      withClue(s"File $path should pass.") {
        ctx.reporter shouldBe errorless
        program should not be (None)
      }
    }

  def assertFileFails(path: String) = checkResultForFile(path) { (ctx, program) =>
      withClue(s"File $path should fail.") {
        ctx.reporter should not be errorless
        program should be (None)
      }
    }

  def assertStringSucceeds(source: String) = checkResultForString(source) { (ctx, program) =>
      withClue(s"The following program should pass:\n$source") {
        ctx.reporter shouldBe errorless
        program should not be (None)
      }
    }

  def assertStringFails(source: String) = checkResultForString(source) { (ctx, program) =>
      withClue(s"The following program should fail:\n$source") {
        ctx.reporter should not be errorless
        program should be (None)
      }
    }
}
