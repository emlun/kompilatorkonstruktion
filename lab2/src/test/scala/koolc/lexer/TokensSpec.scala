package koolc
package lexer

import org.scalatest.FunSpec
import org.scalatest.Matchers

import utils._
import Tokens._

class TokensSpec extends FunSpec with Matchers {

  describe("The Tokens object") {
    describe("has an isPrefix method") {
      it("which is correct for println") {
        isPrefix("", PRINTLN) should be (true)
        isPrefix("p", PRINTLN) should be (true)
        isPrefix("pr", PRINTLN) should be (true)
        isPrefix("pri", PRINTLN) should be (true)
        isPrefix("prin", PRINTLN) should be (true)
        isPrefix("print", PRINTLN) should be (true)
        isPrefix("printl", PRINTLN) should be (true)
        isPrefix("println", PRINTLN) should be (true)

        isPrefix("a", PRINTLN) should be (false)
        isPrefix("println_", PRINTLN) should be (false)
      }

      it("which is correct for identifiers") {
        isPrefix("", IDKIND) should be (true)
        isPrefix("p", IDKIND) should be (true)
        isPrefix("pr", IDKIND) should be (true)
        isPrefix("pri", IDKIND) should be (true)
        isPrefix("prin", IDKIND) should be (true)
        isPrefix("print", IDKIND) should be (true)
        isPrefix("printl", IDKIND) should be (true)
        isPrefix("println", IDKIND) should be (true)

        isPrefix("a", IDKIND) should be (true)
        isPrefix("a_", IDKIND) should be (true)
        isPrefix("a_b", IDKIND) should be (true)
        isPrefix("a9", IDKIND) should be (true)
        isPrefix("println_", IDKIND) should be (true)

        isPrefix("0", IDKIND) should be (false)
        isPrefix("1", IDKIND) should be (false)
        isPrefix("2", IDKIND) should be (false)
        isPrefix("3", IDKIND) should be (false)
        isPrefix("4", IDKIND) should be (false)
        isPrefix("5", IDKIND) should be (false)
        isPrefix("6", IDKIND) should be (false)
        isPrefix("7", IDKIND) should be (false)
        isPrefix("8", IDKIND) should be (false)
        isPrefix("9", IDKIND) should be (false)

        isPrefix("0a", IDKIND) should be (false)
        isPrefix("1a", IDKIND) should be (false)
        isPrefix("2a", IDKIND) should be (false)
        isPrefix("3a", IDKIND) should be (false)
        isPrefix("4a", IDKIND) should be (false)
        isPrefix("5a", IDKIND) should be (false)
        isPrefix("6a", IDKIND) should be (false)
        isPrefix("7a", IDKIND) should be (false)
        isPrefix("8a", IDKIND) should be (false)
        isPrefix("9a", IDKIND) should be (false)

        isPrefix(" ", IDKIND) should be (false)
        isPrefix("a ", IDKIND) should be (false)
        isPrefix(" a", IDKIND) should be (false)
        isPrefix(" a ", IDKIND) should be (false)
      }

      it("which is correct for string literals") {
        isPrefix("\"", STRLITKIND) should be (true)
        isPrefix("\"\"", STRLITKIND) should be (true)
        isPrefix(""""foo""", STRLITKIND) should be (true)
        isPrefix(""""foo"""", STRLITKIND) should be (true)
        isPrefix(""""foo1337_"""", STRLITKIND) should be (true)

        isPrefix("a", STRLITKIND) should be (false)
        isPrefix("""a"foo"""", STRLITKIND) should be (false)
        isPrefix("""a"foo""", STRLITKIND) should be (false)
        isPrefix(""""foo"a""", STRLITKIND) should be (false)
        isPrefix(""""foo"a"""", STRLITKIND) should be (false)
        isPrefix("""a"foo"a"""", STRLITKIND) should be (false)
      }

      it("which is correct for integer literals") {
        isPrefix("\"", INTLITKIND) should be (false)
        isPrefix("\"\"", INTLITKIND) should be (false)
        isPrefix(""""foo""", INTLITKIND) should be (false)
        isPrefix(""""foo"""", INTLITKIND) should be (false)
        isPrefix(""""foo1337_"""", INTLITKIND) should be (false)

        isPrefix("0", INTLITKIND) should be (true)
        isPrefix("1", INTLITKIND) should be (true)
        isPrefix("2", INTLITKIND) should be (true)
        isPrefix("3", INTLITKIND) should be (true)
        isPrefix("4", INTLITKIND) should be (true)
        isPrefix("5", INTLITKIND) should be (true)
        isPrefix("6", INTLITKIND) should be (true)
        isPrefix("7", INTLITKIND) should be (true)
        isPrefix("8", INTLITKIND) should be (true)
        isPrefix("9", INTLITKIND) should be (true)

        isPrefix("00", INTLITKIND) should be (false)
        isPrefix("10", INTLITKIND) should be (true)
        isPrefix("20", INTLITKIND) should be (true)
        isPrefix("30", INTLITKIND) should be (true)
        isPrefix("40", INTLITKIND) should be (true)
        isPrefix("50", INTLITKIND) should be (true)
        isPrefix("60", INTLITKIND) should be (true)
        isPrefix("70", INTLITKIND) should be (true)
        isPrefix("80", INTLITKIND) should be (true)
        isPrefix("90", INTLITKIND) should be (true)

        isPrefix("00", INTLITKIND) should be (false)
        isPrefix("01", INTLITKIND) should be (false)
        isPrefix("02", INTLITKIND) should be (false)
        isPrefix("03", INTLITKIND) should be (false)
        isPrefix("04", INTLITKIND) should be (false)
        isPrefix("05", INTLITKIND) should be (false)
        isPrefix("06", INTLITKIND) should be (false)
        isPrefix("07", INTLITKIND) should be (false)
        isPrefix("08", INTLITKIND) should be (false)
        isPrefix("09", INTLITKIND) should be (false)

        isPrefix("0a", INTLITKIND) should be (false)
        isPrefix("1a", INTLITKIND) should be (false)
        isPrefix("2a", INTLITKIND) should be (false)
        isPrefix("3a", INTLITKIND) should be (false)
        isPrefix("4a", INTLITKIND) should be (false)
        isPrefix("5a", INTLITKIND) should be (false)
        isPrefix("6a", INTLITKIND) should be (false)
        isPrefix("7a", INTLITKIND) should be (false)
        isPrefix("8a", INTLITKIND) should be (false)
        isPrefix("9a", INTLITKIND) should be (false)

        isPrefix("a0", INTLITKIND) should be (false)
        isPrefix("a1", INTLITKIND) should be (false)
        isPrefix("a2", INTLITKIND) should be (false)
        isPrefix("a3", INTLITKIND) should be (false)
        isPrefix("a4", INTLITKIND) should be (false)
        isPrefix("a5", INTLITKIND) should be (false)
        isPrefix("a6", INTLITKIND) should be (false)
        isPrefix("a7", INTLITKIND) should be (false)
        isPrefix("a8", INTLITKIND) should be (false)
        isPrefix("a9", INTLITKIND) should be (false)
      }
    }
  }
}
