package koolc
package lexer

import org.scalatest.FunSpec
import org.scalatest.Matchers

import utils._
import Tokens._

class TokensSpec extends FunSpec with Matchers {

  implicit class TokenMatcher(val kind: TokenKind) {
    def hasPrefix(prefix: String)    = Tokens.isPrefix(prefix, kind) should be (true)
    def hasNotPrefix(prefix: String) = Tokens.isPrefix(prefix, kind) should be (false)
    def matches(word: String)        = Tokens.isToken(word, kind) should be (true)
    def matchesNot(word: String)     = Tokens.isToken(word, kind) should be (false)
  }

  describe("The Tokens object") {
    describe("has an isPrefix method") {
      it("which is correct for println") {
        PRINTLN hasPrefix ""
        PRINTLN hasPrefix "p"
        PRINTLN hasPrefix "pr"
        PRINTLN hasPrefix "pri"
        PRINTLN hasPrefix "prin"
        PRINTLN hasPrefix "print"
        PRINTLN hasPrefix "printl"
        PRINTLN hasPrefix "println"

        PRINTLN hasNotPrefix "a"
        PRINTLN hasNotPrefix "println_"
      }

      it("which is correct for identifiers") {
        IDKIND hasPrefix ""
        IDKIND hasPrefix "p"
        IDKIND hasPrefix "pr"
        IDKIND hasPrefix "pri"
        IDKIND hasPrefix "prin"
        IDKIND hasPrefix "print"
        IDKIND hasPrefix "printl"
        IDKIND hasPrefix "println"

        IDKIND hasPrefix "a"
        IDKIND hasPrefix "a_"
        IDKIND hasPrefix "a_b"
        IDKIND hasPrefix "a9"
        IDKIND hasPrefix "println_"

        IDKIND hasNotPrefix "0"
        IDKIND hasNotPrefix "1"
        IDKIND hasNotPrefix "2"
        IDKIND hasNotPrefix "3"
        IDKIND hasNotPrefix "4"
        IDKIND hasNotPrefix "5"
        IDKIND hasNotPrefix "6"
        IDKIND hasNotPrefix "7"
        IDKIND hasNotPrefix "8"
        IDKIND hasNotPrefix "9"

        IDKIND hasNotPrefix "0a"
        IDKIND hasNotPrefix "1a"
        IDKIND hasNotPrefix "2a"
        IDKIND hasNotPrefix "3a"
        IDKIND hasNotPrefix "4a"
        IDKIND hasNotPrefix "5a"
        IDKIND hasNotPrefix "6a"
        IDKIND hasNotPrefix "7a"
        IDKIND hasNotPrefix "8a"
        IDKIND hasNotPrefix "9a"

        IDKIND hasNotPrefix " "
        IDKIND hasNotPrefix "a "
        IDKIND hasNotPrefix " a"
        IDKIND hasNotPrefix " a "
      }

      it("which is correct for string literals") {
        STRLITKIND hasPrefix "\""
        STRLITKIND hasPrefix "\"\""
        STRLITKIND hasPrefix """"foo"""
        STRLITKIND hasPrefix """"foo""""
        STRLITKIND hasPrefix """"foo1337_""""

        STRLITKIND hasNotPrefix "a"
        STRLITKIND hasNotPrefix """a"foo""""
        STRLITKIND hasNotPrefix """a"foo"""
        STRLITKIND hasNotPrefix """"foo"a"""
        STRLITKIND hasNotPrefix """"foo"a""""
        STRLITKIND hasNotPrefix """a"foo"a""""
      }

      it("which is correct for integer literals") {
        INTLITKIND hasNotPrefix "\""
        INTLITKIND hasNotPrefix "\"\""
        INTLITKIND hasNotPrefix """"foo"""
        INTLITKIND hasNotPrefix """"foo""""
        INTLITKIND hasNotPrefix """"foo1337_""""

        INTLITKIND hasPrefix "0"
        INTLITKIND hasPrefix "1"
        INTLITKIND hasPrefix "2"
        INTLITKIND hasPrefix "3"
        INTLITKIND hasPrefix "4"
        INTLITKIND hasPrefix "5"
        INTLITKIND hasPrefix "6"
        INTLITKIND hasPrefix "7"
        INTLITKIND hasPrefix "8"
        INTLITKIND hasPrefix "9"

        INTLITKIND hasNotPrefix "00"
        INTLITKIND hasPrefix "10"
        INTLITKIND hasPrefix "20"
        INTLITKIND hasPrefix "30"
        INTLITKIND hasPrefix "40"
        INTLITKIND hasPrefix "50"
        INTLITKIND hasPrefix "60"
        INTLITKIND hasPrefix "70"
        INTLITKIND hasPrefix "80"
        INTLITKIND hasPrefix "90"

        INTLITKIND hasNotPrefix "00"
        INTLITKIND hasNotPrefix "01"
        INTLITKIND hasNotPrefix "02"
        INTLITKIND hasNotPrefix "03"
        INTLITKIND hasNotPrefix "04"
        INTLITKIND hasNotPrefix "05"
        INTLITKIND hasNotPrefix "06"
        INTLITKIND hasNotPrefix "07"
        INTLITKIND hasNotPrefix "08"
        INTLITKIND hasNotPrefix "09"

        INTLITKIND hasNotPrefix "0a"
        INTLITKIND hasNotPrefix "1a"
        INTLITKIND hasNotPrefix "2a"
        INTLITKIND hasNotPrefix "3a"
        INTLITKIND hasNotPrefix "4a"
        INTLITKIND hasNotPrefix "5a"
        INTLITKIND hasNotPrefix "6a"
        INTLITKIND hasNotPrefix "7a"
        INTLITKIND hasNotPrefix "8a"
        INTLITKIND hasNotPrefix "9a"

        INTLITKIND hasNotPrefix "a0"
        INTLITKIND hasNotPrefix "a1"
        INTLITKIND hasNotPrefix "a2"
        INTLITKIND hasNotPrefix "a3"
        INTLITKIND hasNotPrefix "a4"
        INTLITKIND hasNotPrefix "a5"
        INTLITKIND hasNotPrefix "a6"
        INTLITKIND hasNotPrefix "a7"
        INTLITKIND hasNotPrefix "a8"
        INTLITKIND hasNotPrefix "a9"
      }
    }
  }
}
