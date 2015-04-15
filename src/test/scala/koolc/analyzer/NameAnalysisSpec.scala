package koolc
package analyzer

import java.io.File
import scala.io.Source
import scala.util.Try

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.scalamock.scalatest.MockFactory

import utils._
import lexer._
import ast._

import Trees._
import Symbols._

class NameAnalysisSpec extends FunSpec with Matchers with ReporterMatchers with SymbolMatchers with MockFactory {

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

  def checkResultForFile(path: String, body: (Context, Option[Program]) => Unit) = {
    val input = Try(new File(getClass.getResource(path).toURI())) getOrElse fail("File " + path + " not found.")
    val pipeline = Lexer andThen Parser andThen NameAnalysis andThen checkResult(body)
    pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
  }
  def checkResultForString(source: String, body: (Context, Option[Program]) => Unit) = {
    val pipeline = SourceLexer andThen Parser andThen NameAnalysis andThen checkResult(body)
    pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
  }
  def assertFileSucceeds(path: String) = checkResultForFile(path, (ctx, program) => {
      withClue("Name analysis should succeed for file " + path) {
        ctx.reporter shouldBe errorless
        program should not be (None)
      }
    })
  def assertFileFails(path: String) = checkResultForFile(path, (ctx, program) => {
      withClue("Name analysis should fail for file " + path) {
        ctx.reporter should not be errorless
        program should be (None)
      }
    })

  def checkRefs(tree: Tree): Unit = tree match {
    case id: Identifier => withClue("Identifier:") { id.symbol  should not be None }
    case ths: This      => withClue("This:")       { ths.symbol should not be None }

    case Program(main, classes) =>                        { checkRefs(main); classes foreach checkRefs _ }
    case MainObject(id, stats)  => withClue("MainObject") { checkRefs(id); stats foreach checkRefs _     }
    case VarDecl(tpe, id)       => withClue("VarDecl")    { checkRefs(tpe); checkRefs(id)                }
    case Formal(tpe, id)        => withClue("Formal")     { checkRefs(tpe); checkRefs(id)                }
    case clazz: ClassDecl       => withClue("ClassDecl")  {
      checkRefs(clazz.id)
      clazz.parent foreach checkRefs _
      clazz.vars foreach checkRefs _
      clazz.methods foreach checkRefs _
    }
    case method: MethodDecl     => withClue("MethodDecl") {
      checkRefs(method.retType)
      checkRefs(method.id)
      method.args foreach checkRefs _
      method.vars foreach checkRefs _
      method.stats foreach checkRefs _
      checkRefs(method.retExpr)
    }

    case Block(substats)              => withClue("Block") { substats foreach checkRefs _ }
    case If(expr, thn, els)           => withClue("If")    {
      checkRefs(expr);
      checkRefs(thn);
      els foreach checkRefs _
    }
    case While(expr, statement)       => withClue("While")       { checkRefs(expr); checkRefs(statement) }
    case Println(expr)                => withClue("Println")     { checkRefs(expr)                       }
    case Assign(id, expr)             => withClue("Assign")      { checkRefs(id); checkRefs(expr)        }
    case ArrayAssign(id, index, expr) => withClue("ArrayAssign") {
      checkRefs(id)
      checkRefs(index)
      checkRefs(expr)
    }

    case And(lhs, rhs)               => withClue("And")         { checkRefs(lhs); checkRefs(rhs)           }
    case Or(lhs, rhs)                => withClue("Or")          { checkRefs(lhs); checkRefs(rhs)           }
    case Plus(lhs, rhs)              => withClue("Plus")        { checkRefs(lhs); checkRefs(rhs)           }
    case Minus(lhs, rhs)             => withClue("Minus")       { checkRefs(lhs); checkRefs(rhs)           }
    case Times(lhs, rhs)             => withClue("Times")       { checkRefs(lhs); checkRefs(rhs)           }
    case Div(lhs, rhs)               => withClue("Div")         { checkRefs(lhs); checkRefs(rhs)           }
    case LessThan(lhs, rhs)          => withClue("LessThan")    { checkRefs(lhs); checkRefs(rhs)           }
    case Equals(lhs, rhs)            => withClue("Equals")      { checkRefs(lhs); checkRefs(rhs)           }
    case ArrayRead(arr, index)       => withClue("ArrayRead")   { checkRefs(arr); checkRefs(index)         }
    case ArrayLength(arr)            => withClue("ArrayLength") { checkRefs(arr)                           }
                                                                // Method name symbols done later
    case MethodCall(obj, meth, args) => withClue("MethodCall")  { checkRefs(obj); args foreach checkRefs _ }
    case NewIntArray(size)           => withClue("NewIntArray") { checkRefs(size)                          }
    case New(tpe)                    => withClue("New")         { checkRefs(tpe)                           }
    case Not(expr)                   => withClue("Not")         { checkRefs(expr)                          }

    case BooleanType() | IntArrayType() | IntType() | StringType() |
         False()       | True()         | IntLit(_) | StringLit(_) => {}
  }

  describe("The name analyzer") {

    describe("attaches symbols to all declarations") {
      VALID_TEST_FILES foreach { path =>
        it(s"in ${path}") {
          checkResultForFile(path, (ctx, program) => {
            ctx.reporter shouldBe errorless
            program should not be None
            program foreach checkDeclarationSymbols _
          })
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
        assertFileFails("redeclared-member.kool")
        assertFileFails("redeclared-method-variable.kool")
        assertFileFails("redeclared-parameter.kool")
        assertFileFails("parameter-shadowed-by-method-variable.kool")
      }

      it("All variables used must be declared.") {
        assertFileFails("undeclared-variable-in-expression.kool")
        assertFileFails("undeclared-variable-in-assignment.kool")
      }

      describe("Shadowing:") {
        it("A local variable in a method can shadow a class member.") {
          val source = """
            object Main { def main(): Unit = {} }
            class Foo {
              var a: Bool;
              def bar(): Bool = {
                var a: Bool;
                a = a;
                return true;
              }
            }
          """
          checkResultForString(source, (ctx, program) => {
            ctx.reporter shouldBe errorless
            program should not be (None)

            val fooClass = program.get.classes.head
            val barMethod = fooClass.methods.head
            barMethod.stats.head match {
              case Assign(assignId, assignValue: Identifier) => {
                assignId should not (haveSameSymbolAs (fooClass.vars.head))
                assignId should haveSameSymbolAs (barMethod.vars.head)
                assignValue should not (haveSameSymbolAs (fooClass.vars.head))
                assignValue should haveSameSymbolAs (barMethod.vars.head)
              }
              case _ => fail("Expected first statement to be assignment, was: " + barMethod.stats.head)
            }
          })
        }

        it("A method parameter can shadow a class member.") {
          val source = """
            object Main { def main(): Unit = {} }
            class Foo {
              var a: Bool;
              def bar(a: Bool): Bool = {
                a = a;
                return true;
              }
            }
          """
          checkResultForString(source, (ctx, program) => {
            ctx.reporter shouldBe errorless
            program should not be (None)

            val fooClass = program.get.classes.head
            val barMethod = fooClass.methods.head
            barMethod.stats.head match {
              case Assign(assignId, assignValue: Identifier) => {
                assignId should not (haveSameSymbolAs (fooClass.vars.head))
                assignId should haveSameSymbolAs (barMethod.args.head)
                assignValue should not (haveSameSymbolAs (fooClass.vars.head))
                assignValue should haveSameSymbolAs (barMethod.args.head)
              }
              case _ => fail("Expected first statement to be assignment, was: " + barMethod.stats.head)
            }
          })
        }

        it("No other type of shadowing is allowed in KOOL.") {
          assertFileFails("redeclared-member.kool")
          assertFileFails("redeclared-method-variable.kool")
          assertFileFails("redeclared-parameter.kool")
          assertFileFails("parameter-shadowed-by-method-variable.kool")
          assertFileFails("variable-declaration-in-block.kool")
        }
      }

      describe("Classes:") {
        it("Classes must be defined only once.") {
          assertFileFails("redeclared-class.kool")
        }

        it("When a class is declared as extending another one, the other class must be declared and cannot be the main object.") {
          assertFileFails("nonexistent-parent-class.kool")
          assertFileFails("main-object-as-parent-class.kool")
          assertFileSucceeds("good-parent-class.kool")
        }

        it("""The transitive closure of the "extends" relation must be irreflexive (no cycles in the inheritance graph).""") {
          assertFileFails("circular-inheritance.kool")
        }

        it("When a class name is used as a type, the class must be declared.") {
          assertFileFails("nonexistent-type-in-class-member.kool")
          assertFileFails("nonexistent-type-in-method-parameter.kool")
          assertFileFails("nonexistent-type-in-method-return-type.kool")
          assertFileFails("nonexistent-type-in-method-variable.kool")
        }

        it("The main object cannot be used as a type.") {
          assertFileFails("main-as-type-in-class-member.kool")
          assertFileFails("main-as-type-in-method-parameter.kool")
          assertFileFails("main-as-type-in-method-return-type.kool")
          assertFileFails("main-as-type-in-method-variable.kool")
        }
      }

      describe("Overloading is not permitted:") {
        it("In a given class, no two methods can have the same name.") {
          assertFileFails("redeclared-method.kool")
        }

        it("In a given class, no method can have the same name as another method defined in a super class, unless overriding applies.") {
          cancel("Test not implemented.")
        }
      }

      describe("Overriding:") {
        it("A method in a given class overrides another one in a super class if they have the same name and the same number of arguments. (Of course this constraint will be tightened once we start checking types.)") {
            cancel("Test not implemented.")
        }
        it("Fields cannot be overridden.") {
            cancel("Test not implemented.")
        }
      }
    }

    it("assigns overriding methods a different symbol than their overridden counterparts.") {
      cancel("Test not implemented.")
    }

    it("does not resolve method name symbols in method calls.") {
      cancel("Test not implemented.")
    }

    it("emits a warning to the user when a declared variable is never accessed (read or written).") {
      val warningSpy = mockFunction[Any, Positioned]
      val reporter = new Reporter {
        override def warning(msg: Any, pos: Positioned = NoPosition): Unit = warningSpy(msg, pos)
      }
      warningSpy expects (*, Identifier("a"))
      warningSpy expects (*, Identifier("b"))
      warningSpy expects (*, Identifier("c"))
      warningSpy expects (*, Formal(BooleanType(), Identifier("d")))
      warningSpy expects (*, Identifier("e"))

      val path = "unused-variables.kool"
      val input = Try(new File(getClass.getResource(path).toURI())) getOrElse fail("File " + path + " not found.")
      val pipeline = Lexer andThen Parser andThen NameAnalysis andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless
        program should not be None
      })
      pipeline.run(Context(reporter = reporter, outDir = None, file = Some(input)))(input)
    }

    describe("attaches symbol references to all identifiers") {
      VALID_TEST_FILES foreach { path =>
        it(s"in ${path}") {
          checkResultForFile(path, (ctx, program) => {
            ctx.reporter shouldBe errorless
            program should not be None
            program foreach checkRefs _
          })
        }
      }
    }

  }
}
