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
      val pairs = left.toSeq.zipAll(expectedTokens, new Token(Tokens.BAD), Tokens.BAD) map (pair => {
        val (actual, expected) = pair
        if(actual.kind != expected)
          (false, s"Mismatch: $actual is not $expected")
        else
          (true, s"Ok: $actual is $expected")
      })
      val message = (pairs map (_._2)).mkString("\n")
      MatchResult(pairs forall (_._1), message, message)
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
