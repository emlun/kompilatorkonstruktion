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

  val TEST_FILES =
    "/helloworld.kool" ::
    "/greeter.kool" ::
    "/noop.kool" ::
    "/plundahl.kool" ::
    "/testprograms/lab3/valid/99bottles.kool" ::
    "/testprograms/lab3/valid/BinarySearch.kool" ::
    "/testprograms/lab3/valid/Calendar.kool" ::
    "/testprograms/lab3/valid/ComplexNumbers.kool" ::
    "/testprograms/lab3/valid/DrawStuff.kool" ::
    "/testprograms/lab3/valid/Factorial.kool" ::
    "/testprograms/lab3/valid/GCD.kool" ::
    "/testprograms/lab3/valid/HeapSort.kool" ::
    "/testprograms/lab3/valid/Life.kool" ::
    "/testprograms/lab3/valid/Multiplicator.kool" ::
    "/testprograms/lab3/valid/NewtonsMethod.kool" ::
    "/testprograms/lab3/valid/OptimalChange.kool" ::
    "/testprograms/lab3/valid/Polymorphism.kool" ::
    "/testprograms/lab3/valid/PrimeTest.kool" ::
    "/testprograms/lab3/valid/QuickSort.kool" ::
    "/testprograms/lab3/valid/ScalarProduct.kool" ::
    "/testprograms/lab3/valid/Simple.kool" ::
    "/testprograms/lab3/valid/Sudoku.kool" ::
    "/testprograms/lab3/valid/VehicleRent.kool" ::
    Nil

  private object PrinterPipeline extends Pipeline[Option[Tree], String] {
    def run(ctx: Context)(tree: Option[Tree]): String = tree map Printer getOrElse "<empty program>"
  }

  describe("The pretty printer") {

    TEST_FILES foreach ((path: String) => {
      it(s"outputs the expected result for ${path}.") {
        val expected = (Source fromURL getClass.getResource(path + ".pretty")).mkString
        val input = new File(getClass.getResource(path).toURI())
        val pipeline = Lexer andThen Parser andThen PrinterPipeline
        val output = pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
        output should be (expected)
      }
    })

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
