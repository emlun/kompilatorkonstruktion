package koolc
package analyzer

import java.io.File
import scala.io.Source
import scala.util.Try

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.scalamock.scalatest.MockFactory

import utils._
import lexer._
import ast._

import Trees._
import Symbols._
import Types._

class TypeCheckingSpec extends FunSpec with TestUtils with Matchers with ReporterMatchers with MockFactory {

  override def pipeline = Parser andThen NameAnalysis andThen TypeChecking

  describe("The type checker") {
    describe("fails programs where") {
      it("test expressions in if statements are not Boolean.") {
        assertFileFails("if-test-int.kool")
        assertFileFails("if-test-string.kool")
        assertFileFails("if-test-array.kool")
        assertFileFails("if-test-class.kool")
      }
      it("test expressions in while statements are not Boolean.") {
        assertFileFails("while-test-int.kool")
        assertFileFails("while-test-string.kool")
        assertFileFails("while-test-array.kool")
        assertFileFails("while-test-class.kool")
      }
      it("either operand of + is not a String or Int.") {
        assertFileFails("plus-array-array.kool")
        assertFileFails("plus-array-array.kool")
        assertFileFails("plus-array-bool.kool")
        assertFileFails("plus-array-class.kool")
        assertFileFails("plus-array-int.kool")
        assertFileFails("plus-array-string.kool")
        assertFileFails("plus-bool-array.kool")
        assertFileFails("plus-bool-bool.kool")
        assertFileFails("plus-bool-class.kool")
        assertFileFails("plus-bool-int.kool")
        assertFileFails("plus-bool-string.kool")
        assertFileFails("plus-class-array.kool")
        assertFileFails("plus-class-bool.kool")
        assertFileFails("plus-class-class.kool")
        assertFileFails("plus-class-int.kool")
        assertFileFails("plus-class-string.kool")
        assertFileFails("plus-int-array.kool")
        assertFileFails("plus-int-bool.kool")
        assertFileFails("plus-int-class.kool")
        assertFileFails("plus-string-array.kool")
        assertFileFails("plus-string-bool.kool")
        assertFileFails("plus-string-class.kool")
      }
      it("the operands of == are of different types.") { cancel("Test not implemented.") }
      it("the LHS operand of a method call expression is not of a class type.") { cancel("Test not implemented.") }
      it("undeclared methods are called.") { cancel("Test not implemented.") }
      it("methods are called with the wrong number of arguments.") { cancel("Test not implemented.") }
      it("methods are called with arguments that are not subtypes of the declared argument types.") { cancel("Test not implemented.") }
      it("assignments are made with values that are not subtypes of the declared variable type.") { cancel("Test not implemented.") }
      it("array assignment indices are not Ints.") { cancel("Test not implemented.") }
      it("array assignment is done with a value that is not an Int.") { cancel("Test not implemented.") }
      it("a returned expression is not a subclass of the declared return type.") { cancel("Test not implemented.") }
      it("the argument to println is not a String, Int or Boolean.") { cancel("Test not implemented.") }
    }
    describe("accepts programs with") {
      it("Boolean test expressions in if statements.") { cancel("Test not implemented.") }
      it("Boolean test expressions in while statements.") { cancel("Test not implemented.") }
      it("arguments to println that are Strings, Ints or Booleans.") { cancel("Test not implemented.") }
      it("+ expressions where both operands are either Int or String.") { cancel("Test not implemented.") }
      it("nontrivial expressions as method call objects.") { cancel("Test not implemented.") }
    }
    it("does not stop at the first type error.") { cancel("Test not implemented.") }
  }
}
