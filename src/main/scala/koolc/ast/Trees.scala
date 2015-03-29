/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast

import utils._
import analyzer.Symbols._

object Trees {
  sealed trait Tree extends Positioned {
    def print: String
  }

  def indent(indentee: String, times: Int = 1): String = indentee.lines map { "  " * times + _ } mkString "\n"
  def trimLinesFromRight(s: String): String = s.lines map { _.replaceFirst("\\s+$", "") } mkString "\n"

  sealed trait SymbolicTree[S <: Symbol] extends Tree with Symbolic[S] {
    def symbolComment: String = symbol map { sym => s"/* ${sym.name}#${sym.id} */ " } getOrElse ""
  }

  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree {
    override def print: String = {
      val mainObject = main.print
      val classesStrings = classes map { _.print } mkString "\n"

      trimLinesFromRight(main.print + "\n" + classesStrings + "\n")
    }
  }
  case class MainObject(id: Identifier, stats: List[StatTree]) extends SymbolicTree[ClassSymbol] {
    override def print: String = {
      val statments = stats map { _.print } mkString "\n"
      val mainMethod = "def main() : Unit = {\n" + indent(statments) + "\n}"
      "object " + id.print + " " + symbolComment + "{\n" + indent(mainMethod) + "\n}\n"
    }
  }
  case class ClassDecl(
      id: Identifier,
      parent: Option[Identifier],
      vars: List[VarDecl],
      methods: List[MethodDecl]) extends Tree with Symbolic[ClassSymbol] {
    override def print: String = {
      val extend = parent map (" extends " + _.print) getOrElse ""
      val vari = vars map { _.print }
      val meti = methods map { _.print }

      val body = vari ++: meti mkString "\n"
      "class " + id.print + extend + " {\n" + indent(body) + "\n}\n"
    }
  }
  case class VarDecl(tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol] {
    override def print: String = s"var ${id.print} : ${tpe.print};"
  }
  case class MethodDecl(
      retType: TypeTree,
      id: Identifier,
      args: List[Formal],
      vars: List[VarDecl],
      stats: List[StatTree],
      retExpr: ExprTree) extends Tree with Symbolic[MethodSymbol] {
    override def print: String = {
      val arg = args map (_.print) mkString ", "
      val vari = vars map { _.print }
      val stmt = stats map { _.print }
      val ret = "return " + retExpr.print + ";"

      val body = (vari ++: stmt ++: List(ret)) mkString "\n"
      "def " + id.print + " ( " + arg + " ) : " + retType.print + " = {\n" + indent(body) + "\n}\n"
    }
  }
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree with Symbolic[VariableSymbol] {
    override def print: String = id.print + " : " + tpe.print
  }

  sealed trait TypeTree extends Tree
  case class IntArrayType() extends TypeTree {
    override def print: String = "Int[ ]"
  }
  case class IntType() extends TypeTree {
    override def print: String = "Int"
  }
  case class BooleanType() extends TypeTree {
    override def print: String = "Bool"
  }
  case class StringType() extends TypeTree {
    override def print: String = "String"
  }

  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree {
    override def print: String = {
      val body = stats map { _.print } mkString "\n"
      "{\n" + indent(body) + "\n}"
    }
  }
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree {
    override def print: String = {
      val addElse = els map { elseStatement =>
          "\nelse\n" + indent(elseStatement.print)
        } getOrElse "";

      "if ( " + expr.print + " )\n" + indent(thn.print) + addElse
    }
  }
  case class While(expr: ExprTree, stat: StatTree) extends StatTree {
    override def print: String = {
      "while ( " + expr.print + " )\n" + indent(stat.print)
    }
  }
  case class Println(expr: ExprTree) extends StatTree {
    override def print: String = "println( " + expr.print + " );"
  }
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree {
    override def print: String = id.print + " = " + expr.print + ";"
  }
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree {
    override def print: String = id.print + "[ " + index.print + " ] = " + expr.print + ";"
  }

  sealed trait ExprTree extends Tree
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print: String = "(" + lhs.print + " && " + rhs.print +")"
  }
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print: String = "(" + lhs.print + " || " + rhs.print +")"
  }
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print: String = "(" + lhs.print + " + " + rhs.print +")"
  }
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print: String = "(" + lhs.print + " - " + rhs.print +")"
  }
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print: String = "(" + lhs.print + " * " + rhs.print +")"
  }
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print: String = "(" + lhs.print + " / " + rhs.print +")"
  }
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print: String = "(" + lhs.print + " < " + rhs.print +")"
  }
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print: String = "(" + lhs.print + " == " + rhs.print +")"
  }
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree {
    override def print: String = arr.print + " [ " + index.print + " ]"
  }
  case class ArrayLength(arr: ExprTree) extends ExprTree {
    override def print: String = arr.print + ".length"
  }
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree {
    override def print: String = {
      val arg = args map (_.print) mkString ", "
      obj.print + "." + meth.print + " ( " + arg + " )"
    }
  }
  case class IntLit(value: Int) extends ExprTree {
    override def print: String = value.toString
  }
  case class StringLit(value: String) extends ExprTree {
    override def print: String = '"' + value + '"'
  }

  case class True() extends ExprTree {
    override def print: String = "true"
  }
  case class False() extends ExprTree {
    override def print: String = "false"
  }
  case class Identifier(value: String) extends TypeTree with ExprTree with Symbolic[Symbol] {
    override def print: String = value
  }

  case class This() extends ExprTree with Symbolic[ClassSymbol] {
    override def print: String = "this"
  }
  case class NewIntArray(size: ExprTree) extends ExprTree {
    override def print: String = "new Int [ " + size.print + " ]"
  }
  case class New(tpe: Identifier) extends ExprTree {
    override def print: String = "new " + tpe.print + "()"
  }

  case class Not(expr: ExprTree) extends ExprTree {
    override def print: String = "!" + expr.print
  }
}
