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

        program map (inside(_) { case Program(main, classes) =>
          classes should be ('empty)

          inside(main) { case MainObject(id, statements) =>
            id.value should be ("foo")

            statements should be (Println(StringLit("Hello, World!")) :: Nil)
          }
        }) orElse fail("Expected program to be defined.")
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
                  retExpr = new This
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

  }
}
