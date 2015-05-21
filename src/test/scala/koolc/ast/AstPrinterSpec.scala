package koolc
package ast

import java.io.File
import scala.io.Source

import org.scalatest.FunSpec
import org.scalatest.Matchers

import lexer._
import utils._
import Trees._

class AstPrinterSpec extends FunSpec with Matchers {

  val TEST_FILES =
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

  private object AstPrinterPipeline extends Pipeline[Option[Tree], String] {
    def run(ctx: Context)(tree: Option[Tree]): String =
      tree map PrintAST map (_ + "\n") getOrElse "<empty program>"
  }

  describe("The AST printer") {

    TEST_FILES foreach ((path: String) => {
      it(s"outputs the same result as the reference compiler for ${path}.") {
        val expected = (Source fromURL getClass.getResource(path + ".ast")).mkString
        val input = new File(getClass.getResource(path).toURI())
        val pipeline = Lexer andThen Parser andThen AstPrinterPipeline
        val output = pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
        output should be (expected)
      }
    })

  }

}
