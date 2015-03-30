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

  def assertFileFails(path: String) = {
    val input = new File(getClass.getResource(path).toURI())
    val pipeline = Lexer andThen Parser andThen NameAnalysis andThen checkResult((ctx, program) => {
      ctx.reporter should not be errorless
      program should be (None)
    })
    pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
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

    describe("detects if") {
      it("classes are declared multiple times.")           { assertFileFails("redeclared-class.kool")           }
      it("a class has the same name as the main object.")  { assertFileFails("redeclared-main.kool")            }
      it("class members are declared multiple times.")     { assertFileFails("redeclared-member.kool")          }
      it("methods are declared multiple times.")           { assertFileFails("redeclared-method.kool")          }
      it("method parameters are declared multiple times.") { assertFileFails("redeclared-parameter.kool")       }
      it("method variables are declared multiple times.")  { assertFileFails("redeclared-method-variable.kool") }
      it("class inheritance is cyclic.")                   { assertFileFails("circular-inheritance.kool")       }
    }

    describe("enforces the constraint:") {
      it("No two variables can have the same name in the same scope, unless one of the two cases of shadowing occurs.") {
        cancel("Test not implemented.")
      }

      it("All variables used must be declared.") {
        cancel("Test not implemented.")
      }

      it("A local variable in a method can shadow a class member.") {
        cancel("Test not implemented.")
      }

      it("A method parameter can shadow a class member.") {
        cancel("Test not implemented.")
      }

      it("No other type of shadowing is allowed in KOOL.") {
        cancel("Test not implemented.")
      }

      it("Classes must be defined only once.") {
        cancel("Test not implemented.")
      }

      it("When a class is declared as extending another one, the other class must be declared and cannot be the main object.") {
        cancel("Test not implemented.")
      }

      it("""The transitive closure of the "extends" relation must be irreflexive (no cycles in the inheritance graph).""") {
        cancel("Test not implemented.")
      }

      it("When a class name is used as a type, the class must be declared.") {
        cancel("Test not implemented.")
      }

      it("The main object cannot be used as a type.") {
        cancel("Test not implemented.")
      }

      describe("Overloading is not permitted:") {
        it("In a given class, no two methods can have the same name.") {
          cancel("Test not implemented.")
        }
        it("In a given class, no method can have the same name as another method defined in a super class, unless overriding applies.") {
          cancel("Test not implemented.")
        }
      }

      it("A method in a given class overrides another one in a super class if they have the same name and the same number of arguments. (Of course this constraint will be tightened once we start checking types.)") {
          cancel("Test not implemented.")
      }
      it("Fields cannot be overridden.") {
          cancel("Test not implemented.")
      }
    }

  }
}
