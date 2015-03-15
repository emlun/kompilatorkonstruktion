package koolc
package ast

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest._
import matchers._

import utils._
import lexer._
import Tokens._

import Trees._

class ParserSpec extends FunSpec with Matchers with Inside {

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

      val program = Parser.run(
          Context(reporter = new koolc.utils.Reporter, outDir = None, file = None)
        )(source.toIterator)

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
}
