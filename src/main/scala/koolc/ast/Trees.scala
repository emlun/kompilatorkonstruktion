/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast

import scala.util.Try

import utils._
import analyzer.Symbols._
import analyzer.Types._

object Trees {
  sealed trait Tree extends Positioned

  def indent(indentee: String, times: Int = 1): String = indentee.lines map { "  " * times + _ } mkString "\n"
  def trimLinesFromRight(s: String): String = s.lines map { _.replaceFirst("\\s+$", "") } mkString "\n"

  sealed trait SymbolicTree[S <: Symbol] extends Tree with Symbolic[S] {
    def symbolComment: String = "#" + (Try(symbol.id) getOrElse "??")
  }

  sealed trait TemplateTree {
    val template: List[Identifier]
  }

  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree {
    def pureClasses: List[ClassDecl] = classes filter { _.template.isEmpty }
  }
  case class MainObject(id: Identifier, stats: List[StatTree]) extends SymbolicTree[ClassSymbol]
  case class ClassDecl(
      id: Identifier,
      parent: Option[Identifier],
      vars: List[VarDecl],
      methods: List[MethodDecl],
      template: List[Identifier] = Nil) extends SymbolicTree[ClassSymbol] with TemplateTree {
    def pureMethods: List[MethodDecl] = methods filter { _.template.isEmpty }
  }
  case class VarDecl(tpe: TypeTree, id: Identifier) extends SymbolicTree[VariableSymbol]
  case class MethodDecl(
      retType: TypeTree,
      id: Identifier,
      args: List[Formal],
      vars: List[VarDecl],
      stats: List[StatTree],
      retExpr: ExprTree,
      template: List[Identifier] = Nil) extends SymbolicTree[MethodSymbol] with TemplateTree
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends SymbolicTree[VariableSymbol]

  sealed trait TypeTree extends Tree with Typed {
    def name: String = this match {
      case IntArrayType()    => "Int[]"
      case IntType()         => "Int"
      case BooleanType()     => "Bool"
      case StringType()      => "String"
      case Identifier(value,template) => value
    }
    def name2: String = this match {
      case Identifier(value,template) => value + (if(!template.isEmpty) "$" + (template map { _.name2 } mkString ",") else "")
      case _ => name
    }
  }
  case class IntArrayType() extends TypeTree
  case class IntType() extends TypeTree
  case class BooleanType() extends TypeTree
  case class StringType() extends TypeTree

  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class Println(expr: ExprTree) extends StatTree
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree

  sealed trait ExprTree extends Tree with Typed
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree
  case class ArrayLength(arr: ExprTree) extends ExprTree
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree
  case class IntLit(value: Int) extends ExprTree
  case class StringLit(value: String) extends ExprTree

  case class True() extends ExprTree
  case class False() extends ExprTree
  case class Identifier(value: String, template: List[TypeTree] = Nil) extends TypeTree with ExprTree with SymbolicTree[Symbol]

  case class This() extends ExprTree with SymbolicTree[ClassSymbol]
  case class NewIntArray(size: ExprTree) extends ExprTree
  case class New(tpe: Identifier) extends ExprTree

  case class Not(expr: ExprTree) extends ExprTree
}
