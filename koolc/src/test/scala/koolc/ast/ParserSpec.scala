package koolc
package ast

import java.io.File
import scala.io.Source

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.Inside

import utils._
import lexer._
import Tokens._

import Trees._

trait ParseMatchers {
  import org.scalatest.matchers._

  class ReporterShouldBeErrorlessMatcher extends BeMatcher[Reporter] {
    override def apply(left: Reporter) = {
      val message = left.messages map (_.toString) mkString "\n"
      MatchResult(!left.hasErrors, message, message)
    }
  }

  def errorless = new ReporterShouldBeErrorlessMatcher
}

class ParserSpec extends FunSpec with Matchers with Inside with ParseMatchers {

  val VALID_TEST_FILES =
    "/helloworld.kool" ::
    "/greeter.kool" ::
    "/noop.kool" ::
    "/plundahl.kool" ::
    "/printed.kool" ::
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

  val INVALID_TEST_FILES =
    "/testprograms/lab2/invalid/002.kool" ::
    "/testprograms/lab2/invalid/005.kool" ::
    "/testprograms/lab2/invalid/007.kool" ::
    "/testprograms/lab3/invalid/XX-Fail02.kool" ::
    "/testprograms/lab3/invalid/XX-Fail05.kool" ::
    "/testprograms/lab3/invalid/XX-Fail08.kool" ::
    "/testprograms/lab3/invalid/XX-Fail10.kool" ::
    "/testprograms/lab3/invalid/XX-Fail12.kool" ::
    "/testprograms/lab3/invalid/XX-Fail15.kool" ::
    "/testprograms/lab3/invalid/XX-Fail18.kool" ::
    "/testprograms/lab3/invalid/XX-Fail24.kool" ::
    "/testprograms/lab3/invalid/XX-Fail33.kool" ::
    "/testprograms/lab3/invalid/XX-Fail36.kool" ::
    "/testprograms/lab3/invalid/XX-Fail39.kool" ::
    Nil

  def checkResult(body: (Context, Option[Program]) => Unit) = new Pipeline[Option[Program], Unit] {
    def run(ctx: Context)(program: Option[Program]) = body(ctx, program)
  }

  describe("The Parser") {

    it("fails on empty input.") {
      (Parser andThen checkResult ((ctx: Context, _) => {
        ctx.reporter should not be errorless
      })).run(Context(reporter = new Reporter, outDir = None, file = None))((new Token(EOF) :: Nil).toIterator)
    }

    it("parses Hello World correctly.") {
      val source: Seq[Token] =
        new Token(OBJECT) :: new ID("foo") :: new Token(LBRACE) ::
          new Token(DEF) :: new Token(MAIN) :: new Token(LPAREN) :: new Token(RPAREN) ::
          new Token(COLON) :: new Token(UNIT) :: new Token(EQSIGN) :: new Token(LBRACE) ::
            new Token(PRINTLN) ::
              new Token(LPAREN) :: new STRLIT("Hello, World!") :: new Token(RPAREN) :: new Token(SEMICOLON) ::
          new Token(RBRACE) ::
        new Token(RBRACE) ::
        new Token(EOF) ::
        Nil

      val pipeline = Parser andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless

        program should be (Some(Program(
          main = MainObject(Identifier("foo"),
            Println(StringLit("Hello, World!")) ::
            Nil
          ),
          classes = Nil
        )))
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(source.toIterator)
    }

    it("parses single-keyword values correctly.") {
      val source: Seq[Token] =
        new Token(OBJECT) :: new ID("foo") :: new Token(LBRACE) ::
          new Token(DEF) :: new Token(MAIN) :: new Token(LPAREN) :: new Token(RPAREN) ::
          new Token(COLON) :: new Token(UNIT) :: new Token(EQSIGN) :: new Token(LBRACE) ::
            new Token(PRINTLN) ::
              new Token(LPAREN) :: new Token(TRUE) :: new Token(RPAREN) :: new Token(SEMICOLON) ::
            new Token(PRINTLN) ::
              new Token(LPAREN) :: new Token(FALSE) :: new Token(RPAREN) :: new Token(SEMICOLON) ::
            new Token(PRINTLN) ::
              new Token(LPAREN) :: new Token(THIS) :: new Token(RPAREN) :: new Token(SEMICOLON) ::
          new Token(RBRACE) ::
        new Token(RBRACE) ::
        new Token(EOF) ::
        Nil

      val pipeline = Parser andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless
        program.get should be (Program(
          main = MainObject(Identifier("foo"), List(Println(new True), Println(new False), Println(new This))),
          classes = Nil
        ))
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(source.toIterator)
    }

    it("parses types correctly.") {
      val source = """
      object Main { def main(): Unit = {} }
      class Foo {
        var b: Bool;
        var i: Int;
        var a: Int[];
        var s: String;
        var f: Foo;
      }
      """

      val pipeline = SourceLexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless
        program.get should be (Program(
          main = MainObject(Identifier("Main"), Nil),
          classes =
            ClassDecl(
              id = Identifier("Foo"), parent = None, methods = Nil,
              vars =
                VarDecl(new BooleanType, Identifier("b")) ::
                VarDecl(new IntType, Identifier("i")) ::
                VarDecl(new IntArrayType, Identifier("a")) ::
                VarDecl(new StringType, Identifier("s")) ::
                VarDecl(Identifier("Foo"), Identifier("f")) ::
                Nil
              ) ::
            Nil
        ))
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
    }

    it("parses equal-priority operators left-associatively.") {
      val source = """
      object Main {
        def main(): Unit = {
          println(10 - 9 + 8 - 7 + 6 - 5 + 4 - 3 + 2 - 1);
          println(10 / 9 * 8 / 7 * 6 / 5 * 4 / 3 * 2 / 1);
          println(10 == 9 < 8 == 7 < 6 == 5 < 4 == 3 < 2 == 1);
          println(1 && 2 && 3);
          println(1 || 2 || 3);
        }
      }
      """

      val pipeline = SourceLexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless
        program.get should be (Program(
          main = MainObject(Identifier("Main"),
            Println(Minus(Plus(Minus(Plus(Minus(Plus(Minus(Plus(Minus(
                              IntLit(10), IntLit(9)),
                            IntLit(8)),
                          IntLit(7)),
                        IntLit(6)),
                      IntLit(5)),
                    IntLit(4)),
                  IntLit(3)),
                IntLit(2)),
              IntLit(1))
            ) ::
            Println(Div(Times(Div(Times(Div(Times(Div(Times(Div(
                              IntLit(10), IntLit(9)),
                            IntLit(8)),
                          IntLit(7)),
                        IntLit(6)),
                      IntLit(5)),
                    IntLit(4)),
                  IntLit(3)),
                IntLit(2)),
              IntLit(1))
            ) ::
            Println(Equals(LessThan(Equals(LessThan(Equals(LessThan(Equals(LessThan(Equals(
                              IntLit(10), IntLit(9)),
                            IntLit(8)),
                          IntLit(7)),
                        IntLit(6)),
                      IntLit(5)),
                    IntLit(4)),
                  IntLit(3)),
                IntLit(2)),
              IntLit(1))
            ) ::
            Println(And(And(IntLit(1), IntLit(2)), IntLit(3))) ::
            Println(Or(Or(IntLit(1), IntLit(2)), IntLit(3))) ::
            Nil),
          classes = Nil
        ))
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
    }

    it("enforces operator priority.") {
      val source = """
      object Main {
        def main(): Unit = {
          println(9 || 8 && 7 < 6 == 5 + 4 - 3 * 2 / !1);
        }
      }
      """

      val pipeline = SourceLexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless
        program.get should be (Program(
          main = MainObject(Identifier("Main"),
            Println(
              Or(IntLit(9),
                And(IntLit(8),
                  Equals(
                    LessThan(IntLit(7), IntLit(6)),
                    Minus(
                      Plus(IntLit(5), IntLit(4)),
                      Div(
                        Times(IntLit(3), IntLit(2)),
                        Not(IntLit(1))
            )))))) ::
            Nil),
          classes = Nil
        ))
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
    }

    it("does not succeed on empty input.") {
      val pipeline = SourceLexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter should not be errorless
        program should be (None)
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString "")
    }

    it("parses a non-trivial program correctly.") {
      val file = new File(getClass.getResource("/greeter.kool").toURI())

      val pipeline = Lexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless
        program.get should be (Program(
          main = MainObject(id = Identifier("HelloWorld"),
            stats =
              Assign(Identifier("greeter"), New(Identifier("Greeter"))) ::
              Assign(Identifier("result"), MethodCall(
                  Identifier("greeter"), Identifier("greet"), List(StringLit("World")))
                ) ::
              Nil
          ),
          classes =
            ClassDecl(id = Identifier("Named"), parent = None,
              vars = List(VarDecl(new StringType, Identifier("name"))),
              methods =
                MethodDecl(retType = new BooleanType, id = Identifier("setName"),
                  args = List(Formal(new StringType, Identifier("newName"))),
                  vars = Nil,
                  stats =
                    Assign(Identifier("name"), Identifier("newName")) ::
                    Nil,
                  retExpr = new True
                ) ::
                Nil
            ) ::
            ClassDecl(id = Identifier("Greeter"), parent = Some(Identifier("Named")),
              vars = Nil,
              methods =
                MethodDecl(retType = new BooleanType, id = Identifier("greet"),
                  args = List(Formal(new StringType, Identifier("greetee"))),
                  vars = List(VarDecl(new StringType, Identifier("message"))),
                  stats =
                    Assign(Identifier("message"),
                      Plus(
                        Plus(
                          Plus(
                            Plus(StringLit("Hello, "), Identifier("greetee")),
                            StringLit(" from ")
                          ),
                          Identifier("name")
                        ),
                        StringLit("!")
                      )
                    ) ::
                    Println(Identifier("message")) ::
                    Nil,
                  retExpr = new True
                ) ::
                MethodDecl(retType = new BooleanType, id = Identifier("greetTwo"),
                  args =
                    Formal(new StringType, Identifier("greetee1")) ::
                    Formal(new StringType, Identifier("greetee2")) ::
                    Nil,
                  vars = Nil,
                  stats = Nil,
                  retExpr = And(
                    MethodCall(new This, Identifier("greet"), List(Identifier("greetee1"))),
                    MethodCall(new This, Identifier("greet"), List(Identifier("greetee2")))
                    )
                ) ::
                MethodDecl(retType = new BooleanType, id = Identifier("greetYears"),
                  args = List(Formal(new IntArrayType, Identifier("years"))),
                  vars =
                    VarDecl(new IntType, Identifier("i")) ::
                    VarDecl(new BooleanType, Identifier("allSucceeded")) ::
                    Nil,
                  stats =
                    Assign(Identifier("i"), IntLit(0)) ::
                    If(new True, Block(Nil), None) ::
                    While(LessThan(Identifier("i"), ArrayLength(Identifier("years"))), Block(
                      Assign(Identifier("allSucceeded"), And(
                        Identifier("allSucceeded"),
                        MethodCall(new This, Identifier("greet"),
                          Plus(StringLit("Year "), ArrayRead(Identifier("years"), Identifier("i"))) ::
                          Nil
                        )
                      )) ::
                      Assign(Identifier("i"), Plus(Identifier("i"), IntLit(1))) ::
                      Nil
                      )
                    ) ::
                    Nil,
                  retExpr = Identifier("allSucceeded")
                ) ::
                Nil
            ) ::
            Nil
        ))
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(file)))(file)
    }

    it("parses a one-of-everything program correctly.") {
      val file = new File(getClass.getResource("/plundahl.kool").toURI())

      val pipeline = Lexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless

        program map (inside(_) { case Program(main, classes) =>
          classes should not be ('empty)

          inside(main) { case MainObject(id, statements) =>
            id.value should be ("biggerTest")
          }
        }) orElse fail("Expected program to be defined.")
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(file)))(file)
    }

    it("does not crash if an assignment statement is incomplete.") {
      val source = "object Main { def main(): Unit = { foo"
      val pipeline = SourceLexer andThen Parser
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
    }

    it("does not enter an infinite loop if a method call is missing its parentheses.") {
      val source = "object Main { def main(): Unit = { foo.bar; } }"
      val pipeline = SourceLexer andThen Parser
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
    }

    it("parses chained dot operators correctly.") {
      val source = """
      object Main {
        def main(): Unit = {
          result = foo.bar("someArg").length.doSomething().doAnotherThing(true, "two", 3);
        }
      }
      """

      val pipeline = SourceLexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless
        program should be (Some(Program(
          main = MainObject(Identifier("Main"),
            Assign(Identifier("result"),
              MethodCall(
                MethodCall(
                  ArrayLength(
                    MethodCall(
                      Identifier("foo"),
                      Identifier("bar"),
                      List(StringLit("someArg"))
                    )
                  ),
                  Identifier("doSomething"),
                  Nil
                ),
                Identifier("doAnotherThing"),
                List(new True, StringLit("two"), IntLit(3))
              )
            ) ::
            Nil
          ),
          classes = Nil
        )))
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
    }

    it("fails if comma in method call is not followed by an argument.") {
      val source = """
      object Main {
        def main(): Unit = {
          result = foo.bar("someArg",);
        }
      }
      """

      val pipeline = SourceLexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter should not be errorless
        program should be (None)
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
    }

    it("fails if there are trailing characters after the last class declaration.") {
      val source = """
      object Main {
        def main(): Unit = {
          result = foo.bar("someArg",);
        }
      }
      a
      """

      val pipeline = SourceLexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter should not be errorless
        program should be (None)
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
    }

    it("binds else statements to the closest if statement.") {
      val source = """
      object Main {
        def main(): Unit = {
          if(true)
          if(false) {
            println("This statement is false.");
          } else
            println("This statement is true.");

          if(false) {
            if(true)
              println("This statement is true.");
          } else
            println("This statement is false.");
        }
      }
      """

      val pipeline = SourceLexer andThen Parser andThen checkResult((ctx, program) => {
        ctx.reporter shouldBe errorless
        program.get should be (Program(
          main = MainObject(Identifier("Main"),
            If(True(),
              If(False(),
                Block(List(Println(StringLit("This statement is false.")))),
                Some(Println(StringLit("This statement is true.")))
              ),
              None
            ) ::
            If(False(),
              Block(List(If(True(), Println(StringLit("This statement is true.")), None))),
              Some(Println(StringLit("This statement is false.")))
            ) ::
            Nil),
          classes = Nil
        ))
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(Source fromString source)
    }

    describe("successfully parses") {
      VALID_TEST_FILES foreach ((path: String) => {
        it(path) {
          val input = new File(getClass.getResource(path).toURI())
          val pipeline = Lexer andThen Parser andThen checkResult((ctx, program) => {
            ctx.reporter shouldBe errorless
            program should not be (None)
          })
          pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
        }
      })
    }

    describe("does not successfully parse") {
      INVALID_TEST_FILES foreach ((path: String) => {
        it(path) {
          val input = new File(getClass.getResource(path).toURI())
          val pipeline = Lexer andThen Parser andThen checkResult((ctx, program) => {
            ctx.reporter should not be errorless
            program should be (None)
          })
          pipeline.run(Context(reporter = new Reporter, outDir = None, file = Some(input)))(input)
        }
      })
    }

  }
}
