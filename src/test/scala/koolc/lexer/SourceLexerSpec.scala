package koolc
package lexer

import scala.io.Source

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest._
import matchers._

import utils._

trait TokenMatchers {

  class TokenIteratorMatchesTokenKindsMatcher(val expected: Seq[TokenKind])
      extends Matcher[Iterator[Token]] {

    def apply(left: Iterator[Token]) = {
      val leftSeq = left.toSeq
      val pairs = leftSeq.zip(expected) map (pair => {
        val (actual, expected) = pair
        if(actual.kind == expected)
          (true, s"Ok: $actual is $expected")
        else
          (false, s"Kind mismatch: $actual is not $expected")
      })

      val messages = pairs map (_._2)

      val (sameLength, lengthDiffMessages) =
        if(leftSeq.size > expected.size)
          (false, leftSeq drop expected.size map (t => s"Unexpected token found: $t"))
        else if(leftSeq.size < expected.size)
          (false, expected drop leftSeq.size map (t => s"Expected token kind not found: $t"))
        else
          (true, Nil)

      val message = (messages ++ lengthDiffMessages) mkString "\n"
      MatchResult(sameLength && (pairs forall (_._1)), message, message)
    }
  }

  class TokenIteratorMatchesTokensMatcher(val expected: Seq[Token])
      extends Matcher[Iterator[Token]] {

    def apply(left: Iterator[Token]) = {
      val leftSeq = left.toSeq
      val pairs = leftSeq.zip(expected) map (pair => {
        val (actual, expected) = pair
        if(actual.kind == expected.kind) {
          import Tokens.Value
          if(Value(actual) == Value(expected))
            (true, s"Ok: $actual is $expected")
          else
            (false, s"Value mismatch: $actual is not $expected")
        } else {
          (false, s"Kind mismatch: $actual is not $expected")
        }
      })

      val messages = pairs map (_._2)

      val (sameLength, lengthDiffMessages) =
        if(leftSeq.size > expected.size)
          (false, leftSeq drop expected.size map (t => s"Unexpected token found: $t"))
        else if(leftSeq.size < expected.size)
          (false, expected drop leftSeq.size map (t => s"Expected token kind not found: $t"))
        else
          (true, Nil)

      val message = (messages ++ lengthDiffMessages) mkString "\n"
      MatchResult(sameLength && (pairs forall (_._1)), message, message)
    }
  }

  def beKinds(expectedTokens: Seq[TokenKind]) = new TokenIteratorMatchesTokenKindsMatcher(expectedTokens)
  def beTokens(expectedTokens: Seq[Token]) = new TokenIteratorMatchesTokensMatcher(expectedTokens)
}

class SourceLexerSpec extends FunSpec with Matchers with TokenMatchers {

  describe("The SourceLexer") {
    import Tokens._

    def lexed(s: String) = {
      val source = Source fromString s
      SourceLexer.run(
          Context(reporter = new koolc.utils.Reporter, file = None, outDir = None)
        )(source)
    }

    it("lexes Hello World correctly") {
      val source = """println("Hello, World!")"""
      lexed(source) should beKinds (PRINTLN :: LPAREN :: STRLITKIND :: RPAREN :: EOF :: Nil)
    }

    it("lexes Hello World with whitespace correctly") {
      val source = """println ( "Hello, World!" ) """
      lexed(source) should beKinds (PRINTLN :: LPAREN :: STRLITKIND :: RPAREN :: EOF :: Nil)
    }

    it("lexes Hello World with faulty tokens correctly") {
      val source = """println ( & "Hello, World!" ) """
      lexed(source) should beKinds (PRINTLN :: LPAREN :: BAD :: STRLITKIND :: RPAREN :: EOF :: Nil)
    }

    it("lexes int-identifier") {
      val source = """111aaa"""
      lexed(source) should beKinds (INTLITKIND :: IDKIND :: EOF :: Nil)
    }

    it("ignores comments") {
      val source = """
// This is a comment
b

// This is a comment
// This is a comment

      println// This is a comment - skip this
      a/*
        This is also a comment - skip this too
        * // /*
        */p/**//* */a(
      """
      lexed(source) should beKinds (IDKIND :: PRINTLN :: IDKIND :: IDKIND :: IDKIND :: LPAREN :: EOF :: Nil)
    }

    it("does not support nested block comments") {
      val source = """
      println// This is a comment - skip this
      a/*
        This is also a comment /* Nested comment */ - skip this too
        * // /*
        */pa(
      """
      lexed(source) should beKinds (
        PRINTLN ::
        IDKIND ::
        MINUS :: IDKIND :: THIS :: IDKIND ::
        TIMES ::
        TIMES :: DIV :: IDKIND :: LPAREN :: EOF :: Nil)
    }

    it("removes the surrounding quotes from string literal values") {
      val source = """ println("Hello, World!")"""
      lexed(source) should beTokens (
        new Token(PRINTLN) :: new Token(LPAREN) :: new STRLIT("Hello, World!") ::
        new Token(RPAREN) :: new Token(EOF) :: Nil)
    }
  }

}
