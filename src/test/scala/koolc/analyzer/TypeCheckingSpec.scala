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
      it("the operands of == are of different primitive types.") {
        assertFileFails("equals-array-bool.kool")
        assertFileFails("equals-array-int.kool")
        assertFileFails("equals-array-string.kool")
        assertFileFails("equals-bool-array.kool")
        assertFileFails("equals-bool-int.kool")
        assertFileFails("equals-bool-string.kool")
        assertFileFails("equals-int-array.kool")
        assertFileFails("equals-int-bool.kool")
        assertFileFails("equals-int-string.kool")
        assertFileFails("equals-string-array.kool")
        assertFileFails("equals-string-bool.kool")
        assertFileFails("equals-string-int.kool")
      }
      it("exactly one operand of == is of primitive type.") {
        assertFileFails("equals-array-class.kool")
        assertFileFails("equals-bool-class.kool")
        assertFileFails("equals-int-class.kool")
        assertFileFails("equals-string-class.kool")
        assertFileFails("equals-class-array.kool")
        assertFileFails("equals-class-bool.kool")
        assertFileFails("equals-class-int.kool")
        assertFileFails("equals-class-string.kool")
      }

      it("the LHS operand of a method call expression is not of a class type.") {
        assertFileFails("call-method-on-array.kool")
        assertFileFails("call-method-on-bool.kool")
        assertFileFails("call-method-on-int.kool")
        assertFileFails("call-method-on-string.kool")
      }
      it("undeclared methods are called.") {
        assertFileFails("call-undeclared-method.kool")
      }
      it("methods are called with the wrong number of arguments.") { cancel("Test not implemented.") }
      it("methods are called with arguments that are not subtypes of the declared argument types.") { cancel("Test not implemented.") }
      it("assignments are made with values that are not subtypes of the declared variable type.") { cancel("Test not implemented.") }
      it("array assignment indices are not Ints.") { cancel("Test not implemented.") }
      it("array assignment is done with a value that is not an Int.") { cancel("Test not implemented.") }
      it("a returned expression is not a subclass of the declared return type.") { cancel("Test not implemented.") }
      it("the argument to println is not a String, Int or Boolean.") { cancel("Test not implemented.") }
    }
    describe("accepts programs with") {
      it("Boolean test expressions in if statements.") {
        assertFileSucceeds("if-test-bool.kool")
      }
      it("Boolean test expressions in while statements.") {
        assertFileSucceeds("while-test-bool.kool")
      }
      it("arguments to println that are Strings, Ints or Booleans.") { cancel("Test not implemented.") }
      it("+ expressions where both operands are either Int or String.") {
        assertFileSucceeds("plus-int-int.kool")
        assertFileSucceeds("plus-int-string.kool")
        assertFileSucceeds("plus-string-int.kool")
        assertFileSucceeds("plus-string-string.kool")
      }
      it("both operands of == are of class types.") {
        assertFileSucceeds("equals-class1-class2.kool")
        assertFileSucceeds("equals-class1-subclass1.kool")
        assertFileSucceeds("equals-subclass1-class1.kool")
      }
      it("nontrivial expressions as method call objects.") { cancel("Test not implemented.") }
    }
    it("does not stop at the first type error.") { cancel("Test not implemented.") }
  }
}
