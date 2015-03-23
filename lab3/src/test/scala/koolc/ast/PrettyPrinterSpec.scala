package koolc
package ast

import java.io.File
import scala.io.Source

import org.scalatest.FunSpec
import org.scalatest.Matchers

import lexer._
import utils._
import Trees._

class PrettyPrinterSpec extends FunSpec with Matchers {

  private object PrinterPipeline extends Pipeline[Option[Tree], String] {
    def run(ctx: Context)(tree: Option[Tree]): String = tree map Printer getOrElse "<empty program>"
  }

  describe("The pretty printer") {

    it("outputs the expected result for helloworld.kool.") {
      val expected = (Source fromURL getClass.getResource("/helloworld.kool.pretty")).mkString
      val input = new File(getClass.getResource("/helloworld.kool").toURI())
      val pipeline = Lexer andThen Parser andThen PrinterPipeline
      val output = pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
      output should be (expected)
    }

    it("outputs the expected result for greeter.kool.") {
      val expected = (Source fromURL getClass.getResource("/greeter.kool.pretty")).mkString
      val input = new File(getClass.getResource("/greeter.kool").toURI())
      val pipeline = Lexer andThen Parser andThen PrinterPipeline
      val output = pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
      output should be (expected)
    }

    it("outputs the expected result for plundahl.kool.") {
      val expected = (Source fromURL getClass.getResource("/plundahl.kool.pretty")).mkString
      val input = new File(getClass.getResource("/plundahl.kool").toURI())
      val pipeline = Lexer andThen Parser andThen PrinterPipeline
      val output = pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
      output should be (expected)
    }

    it("outputs the expected result for noop.kool.") {
      val expected = (Source fromURL getClass.getResource("/noop.kool.pretty")).mkString
      val input = new File(getClass.getResource("/noop.kool").toURI())
      val pipeline = Lexer andThen Parser andThen PrinterPipeline
      val output = pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
      output should be (expected)
    }

    it("outputs the same thing if run on its own output.") {
      val input = new File(getClass.getResource("/greeter.kool").toURI())
      val firstPipeline = Lexer andThen Parser andThen PrinterPipeline
      val secondPipeline = SourceLexer andThen Parser andThen PrinterPipeline

      val firstOutput = firstPipeline.run(Context(new Reporter, None, Some(input)))(input)
      val secondOutput = secondPipeline.run(Context(new Reporter, None, None))(Source fromString firstOutput)

      secondOutput should be (firstOutput)
    }

  }

}
