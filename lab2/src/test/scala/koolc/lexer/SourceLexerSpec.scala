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

  class TokenIteratorMatchesExpectedTokensMatcher(expected: Seq[TokenKind])
      extends Matcher[Iterator[Token]] {

    def apply(left: Iterator[Token]) = {
      val leftSeq = left.toSeq
      val pairs = leftSeq.zip(expected) map (pair => {
        val (actual, expected) = pair
        if(actual.kind != expected)
          (false, s"Mismatch: $actual is not $expected")
        else
          (true, s"Ok: $actual is $expected")
      })

      val messages = pairs map (_._2)

      val (sameLength, lengthDiffMessages) =
        if(leftSeq.size > expected.size)
          (false, leftSeq  drop (leftSeq.size - expected.size) map (t => s"Unexpected token found: $t"))
        else if(leftSeq.size < expected.size)
          (false, expected drop (expected.size - leftSeq.size) map (t => s"Expected token kind not found: $t"))
        else
          (true, Nil)

      val message = (messages ++ lengthDiffMessages) mkString "\n"
      MatchResult(sameLength && (pairs forall (_._1)), message, message)
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

    it("ignores comments") {
      val source = """
      println// This is a comment - skip this
      a/*
        This is also a comment - skip this too
        * // /*
        */pa(
      """
      lexer.run(ctx)(source) should beTokens (PRINTLN :: IDKIND :: IDKIND :: LPAREN :: EOF :: Nil)
    }
  }

}
