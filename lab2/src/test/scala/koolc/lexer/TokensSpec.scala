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

    describe("has an isToken method") {
      it("which is correct for println") {
        PRINTLN matches "println"

        PRINTLN matchesNot ""
        PRINTLN matchesNot "p"
        PRINTLN matchesNot "pr"
        PRINTLN matchesNot "pri"
        PRINTLN matchesNot "prin"
        PRINTLN matchesNot "print"
        PRINTLN matchesNot "printl"
        PRINTLN matchesNot "println_"
        PRINTLN matchesNot "a"
      }

      it("which is correct for identifiers") {
        IDKIND matchesNot ""
        IDKIND matches "p"
        IDKIND matches "pr"
        IDKIND matches "pri"
        IDKIND matches "prin"
        IDKIND matches "print"
        IDKIND matches "printl"

        IDKIND hasPrefix "println"

        IDKIND matches "a"
        IDKIND matches "a_"
        IDKIND matches "a_b"
        IDKIND matches "a9"
        IDKIND matches "println_"

        IDKIND matchesNot "0"
        IDKIND matchesNot "1"
        IDKIND matchesNot "2"
        IDKIND matchesNot "3"
        IDKIND matchesNot "4"
        IDKIND matchesNot "5"
        IDKIND matchesNot "6"
        IDKIND matchesNot "7"
        IDKIND matchesNot "8"
        IDKIND matchesNot "9"

        IDKIND matchesNot "0a"
        IDKIND matchesNot "1a"
        IDKIND matchesNot "2a"
        IDKIND matchesNot "3a"
        IDKIND matchesNot "4a"
        IDKIND matchesNot "5a"
        IDKIND matchesNot "6a"
        IDKIND matchesNot "7a"
        IDKIND matchesNot "8a"
        IDKIND matchesNot "9a"

        IDKIND matchesNot " "
        IDKIND matchesNot "a "
        IDKIND matchesNot " a"
        IDKIND matchesNot " a "
      }

      it("which is correct for string literals") {
        STRLITKIND matches "\"\""
        STRLITKIND matches """"foo""""
        STRLITKIND matches """"foo1337_""""

        STRLITKIND matchesNot "\""
        STRLITKIND matchesNot "a"
        STRLITKIND matchesNot """"foo"""
        STRLITKIND matchesNot """a"foo""""
        STRLITKIND matchesNot """a"foo"""
        STRLITKIND matchesNot """"foo"a"""
        STRLITKIND matchesNot """"foo"a""""
        STRLITKIND matchesNot """a"foo"a""""
      }

      it("which is correct for integer literals") {
        INTLITKIND matchesNot "\""
        INTLITKIND matchesNot "\"\""
        INTLITKIND matchesNot """"foo"""
        INTLITKIND matchesNot """"foo""""
        INTLITKIND matchesNot """"foo1337_""""

        INTLITKIND matches "0"
        INTLITKIND matches "1"
        INTLITKIND matches "2"
        INTLITKIND matches "3"
        INTLITKIND matches "4"
        INTLITKIND matches "5"
        INTLITKIND matches "6"
        INTLITKIND matches "7"
        INTLITKIND matches "8"
        INTLITKIND matches "9"

        INTLITKIND matchesNot "00"
        INTLITKIND matches "10"
        INTLITKIND matches "20"
        INTLITKIND matches "30"
        INTLITKIND matches "40"
        INTLITKIND matches "50"
        INTLITKIND matches "60"
        INTLITKIND matches "70"
        INTLITKIND matches "80"
        INTLITKIND matches "90"

        INTLITKIND matchesNot "00"
        INTLITKIND matchesNot "01"
        INTLITKIND matchesNot "02"
        INTLITKIND matchesNot "03"
        INTLITKIND matchesNot "04"
        INTLITKIND matchesNot "05"
        INTLITKIND matchesNot "06"
        INTLITKIND matchesNot "07"
        INTLITKIND matchesNot "08"
        INTLITKIND matchesNot "09"

        INTLITKIND matchesNot "0a"
        INTLITKIND matchesNot "1a"
        INTLITKIND matchesNot "2a"
        INTLITKIND matchesNot "3a"
        INTLITKIND matchesNot "4a"
        INTLITKIND matchesNot "5a"
        INTLITKIND matchesNot "6a"
        INTLITKIND matchesNot "7a"
        INTLITKIND matchesNot "8a"
        INTLITKIND matchesNot "9a"

        INTLITKIND matchesNot "a0"
        INTLITKIND matchesNot "a1"
        INTLITKIND matchesNot "a2"
        INTLITKIND matchesNot "a3"
        INTLITKIND matchesNot "a4"
        INTLITKIND matchesNot "a5"
        INTLITKIND matchesNot "a6"
        INTLITKIND matchesNot "a7"
        INTLITKIND matchesNot "a8"
        INTLITKIND matchesNot "a9"
      }
    }
  }
}
