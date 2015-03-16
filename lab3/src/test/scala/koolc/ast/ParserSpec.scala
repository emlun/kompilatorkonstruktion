package koolc
package ast

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
        new Token(OBJECT) ::
        new ID("foo") ::
        new Token(LBRACE) ::
        new Token(DEF) ::
        new Token(MAIN) ::
        new Token(LPAREN) ::
        new Token(RPAREN) ::
        new Token(COLON) ::
        new Token(UNIT) ::
        new Token(EQSIGN) ::
        new Token(LBRACE) ::
        new Token(PRINTLN) ::
        new Token(LPAREN) ::
        new STRLIT("Hello, World!") ::
        new Token(RPAREN) ::
        new Token(SEMICOLON) ::
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

            statements.size should be (1)
            inside(statements.head) { case Println(expr) =>
              inside(expr) { case StringLit(value) =>
                value should be ("Hello, World!")
              }
            }
          }
        }) orElse fail("Expected program to be defined.")
      })
      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(source.toIterator)
    }

  }
}
