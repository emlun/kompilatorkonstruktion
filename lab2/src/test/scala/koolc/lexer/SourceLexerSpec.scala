package koolc
package lexer

import scala.io.Source

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest._
import matchers._

import utils._

object StringToSource extends Pipeline[String, Source] {
  override def run(ctx : Context)(v : String): Source = Source.fromString(v)
}

trait TokenMatchers {

  class TokenIteratorMatchesExpectedTokensMatcher(expectedTokens: Seq[TokenKind])
      extends Matcher[Iterator[Token]] {

    def apply(left: Iterator[Token]) = {
      var accepted = true
      var messages: Seq[String] = Nil

      left.zipAll(expectedTokens.toIterator, new Token(Tokens.BAD), Tokens.BAD).foreach((pair) => {
        val actual = pair._1
        val expected = pair._2

        if(actual.kind != expected) {
          messages = messages :+ s"Mismatch: ${actual.kind} is not $expected"
          accepted = false
        }
      })
      MatchResult( accepted, messages.mkString("\n"), "Success")
    }
  }

  def beTokens(expectedTokens: Seq[TokenKind]) = new TokenIteratorMatchesExpectedTokensMatcher(expectedTokens)
}

class SourceLexerSpec extends FunSpec with Matchers with TokenMatchers {

  describe("The SourceLexer") {
    import Tokens._

    val ctx = Context(new koolc.utils.Reporter, None, null)
    val lexer = StringToSource andThen SourceLexer

    it("lexes Hello World correctly") {
      val source = """println("Hello, World!")"""
      lexer.run(ctx)(source) should beTokens (PRINTLN :: LPAREN :: STRLITKIND :: RPAREN :: EOF :: Nil)
    }

    it("lexes Hello World with whitespace correctly") {
      val source = """println ( "Hello, World!" ) """
      lexer.run(ctx)(source) should beTokens (PRINTLN :: LPAREN :: STRLITKIND :: RPAREN :: EOF :: Nil)
    }

    it("lexes Hello World with faulty tokens correctly") {
      val source = """println ( & "Hello, World!" ) """
      lexer.run(ctx)(source) should beTokens (PRINTLN :: LPAREN :: BAD :: STRLITKIND :: RPAREN :: EOF :: Nil)
    }

    it("lexes int-identifier") {
      val source = """111aaa"""
      lexer.run(ctx)(source) should beTokens (INTLITKIND :: IDKIND :: EOF :: Nil)
    }
  }

}
