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

  describe("The Parser") {

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
        new Token(RBRACE) ::
        new Token(RBRACE) ::
        new Token(EOF) ::
        Nil

      val pipeline = Parser andThen new Pipeline[Program, Unit] {
        def run(ctx: Context)(program: Program) = {
          ctx.reporter shouldBe errorless

          inside(program) { case Program(main, classes) =>
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
          }
        }
      }

      pipeline.run(Context(reporter = new Reporter, outDir = None, file = None))(source.toIterator)
    }

  }
}