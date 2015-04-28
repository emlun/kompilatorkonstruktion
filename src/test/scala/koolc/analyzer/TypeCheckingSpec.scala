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
      it("methods are called with the wrong number of arguments.") {
        assertFileFails("call-with-too-few-args.kool")
        assertFileFails("call-with-too-many-args.kool")
      }
      it("methods are called with arguments that are not subtypes of the declared argument types.") {
        assertFileFails("call-array-with-bool.kool")
        assertFileFails("call-array-with-class.kool")
        assertFileFails("call-array-with-int.kool")
        assertFileFails("call-array-with-string.kool")
        assertFileFails("call-bool-with-array.kool")
        assertFileFails("call-bool-with-class.kool")
        assertFileFails("call-bool-with-int.kool")
        assertFileFails("call-bool-with-string.kool")
        assertFileFails("call-class-with-array.kool")
        assertFileFails("call-class-with-bool.kool")
        assertFileFails("call-class-with-int.kool")
        assertFileFails("call-class-with-string.kool")
        assertFileFails("call-class1-with-class2.kool")
        assertFileFails("call-int-with-array.kool")
        assertFileFails("call-int-with-bool.kool")
        assertFileFails("call-int-with-class.kool")
        assertFileFails("call-int-with-string.kool")
        assertFileFails("call-string-with-array.kool")
        assertFileFails("call-string-with-bool.kool")
        assertFileFails("call-string-with-class.kool")
        assertFileFails("call-string-with-int.kool")
        assertFileFails("call-with-wrong-argument-types.kool")
      }
      it("assignments are made with values that are not subtypes of the declared variable type.") {
        assertFileFails("assign-array-bool.kool")
        assertFileFails("assign-array-class.kool")
        assertFileFails("assign-array-int.kool")
        assertFileFails("assign-array-string.kool")
        assertFileFails("assign-bool-array.kool")
        assertFileFails("assign-bool-class.kool")
        assertFileFails("assign-bool-int.kool")
        assertFileFails("assign-bool-string.kool")
        assertFileFails("assign-class-array.kool")
        assertFileFails("assign-class-bool.kool")
        assertFileFails("assign-class-int.kool")
        assertFileFails("assign-class-string.kool")
        assertFileFails("assign-int-array.kool")
        assertFileFails("assign-int-bool.kool")
        assertFileFails("assign-int-class.kool")
        assertFileFails("assign-int-string.kool")
        assertFileFails("assign-string-array.kool")
        assertFileFails("assign-string-bool.kool")
        assertFileFails("assign-string-class.kool")
        assertFileFails("assign-string-int.kool")
      }
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
      it("method calls where arguments are subclasses of the declared types.") {
        assertFileSucceeds("call-array-with-array.kool")
        assertFileSucceeds("call-bool-with-bool.kool")
        assertFileSucceeds("call-class1-with-class1.kool")
        assertFileSucceeds("call-class1-with-subclass1.kool")
        assertFileSucceeds("call-class1-with-subsubclass1.kool")
        assertFileSucceeds("call-int-with-int.kool")
        assertFileSucceeds("call-string-with-string.kool")
        assertFileSucceeds("call-with-right-argument-types.kool")
      }
      it("assignments where the value is a subclass of the declared type.") {
        assertFileSucceeds("assign-array-array.kool")
        assertFileSucceeds("assign-bool-bool.kool")
        assertFileSucceeds("assign-class1-class1.kool")
        assertFileSucceeds("assign-class1-subclass1.kool")
        assertFileSucceeds("assign-int-int.kool")
        assertFileSucceeds("assign-string-string.kool")
      }
      it("nontrivial expressions as method call objects.") {
        assertFileSucceeds("call-nontrivial-lhs.kool")
      }
    }
    it("does not stop at the first type error.") { cancel("Test not implemented.") }
  }
}
