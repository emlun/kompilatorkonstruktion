package koolc
package analyzer

import java.io.File

import org.scalatest.FunSpec
import org.scalatest.Matchers

import utils._
import lexer._
import ast._

import Trees._
import Symbols._

class NameAnalysisSpec extends FunSpec with Matchers with ReporterMatchers {

  val VALID_TEST_FILES =
    "/helloworld.kool" ::
    "/noop.kool" ::
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

  def checkResult(body: (Context, Option[Program]) => Unit) = new Pipeline[Option[Program], Unit] {
    def run(ctx: Context)(program: Option[Program]) = body(ctx, program)
  }

  def checkSymbolicSymbol[S <: Symbol](symbolic: Symbolic[S]): Unit =
    withClue("Symbol:") { symbolic.symbol should not be None }

  def checkDeclarationSymbols(tree: Tree): Unit = tree match {
    case Program(main, classes) => { checkDeclarationSymbols(main); classes foreach checkDeclarationSymbols _ }
    case main: MainObject       => withClue("MainObject") { checkSymbolicSymbol(main)   }
    case v: VarDecl             => withClue("VarDecl")    { checkSymbolicSymbol(v)      }
    case formal: Formal         => withClue("Formal")     { checkSymbolicSymbol(formal) }
    case clazz: ClassDecl       => withClue("ClassDecl") {
      checkSymbolicSymbol(clazz)
      clazz.vars    foreach checkDeclarationSymbols _
      clazz.methods foreach checkDeclarationSymbols _
    }
    case method: MethodDecl     => withClue("MethodDecl") {
      checkSymbolicSymbol(method)
      method.args foreach checkDeclarationSymbols _
      method.vars foreach checkDeclarationSymbols _
    }
    case t: ExprTree => {}
    case t: StatTree => {}
    case t: TypeTree => {}
  }

  describe("The name analyzer") {

    describe("attaches symbols to all declarations") {
      VALID_TEST_FILES foreach { path =>
        it(s"in ${path}") {
          val input = new File(getClass.getResource(path).toURI())

          val pipeline = Lexer andThen Parser andThen NameAnalysis andThen checkResult((ctx, program) => {
            ctx.reporter shouldBe errorless
            program should not be None
            program foreach checkDeclarationSymbols _
          })
          pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
        }
      }
    }

  }
}
