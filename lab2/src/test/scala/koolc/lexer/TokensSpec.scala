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
      }
    }
  }
}
