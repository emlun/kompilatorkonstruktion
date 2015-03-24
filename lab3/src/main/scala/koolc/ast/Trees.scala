package koolc
package ast

import utils._

object Trees {
  sealed trait Tree extends Positioned {
    def print(level: Int = 0): String
    def indent(times: Int): String = " " * times
  }

  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree {
    override def print(level: Int = 0): String = main.print(1) + (classes map ("\n" + _.print()) mkString "")
  }
  case class MainObject(id: Identifier, stats: List[StatTree]) extends Tree {
    override def print(level: Int = 0): String = {
      val statments = stats map ("\n" + indent(level + 1) + _.print(1)) mkString ""
      s"""object ${id.print()} {
${this.indent(level)}def main() : Unit = {${statments}
${this.indent(level)}}
}
"""
    }
  }
  case class ClassDecl(
      id: Identifier,
      parent: Option[Identifier],
      vars: List[VarDecl],
      methods: List[MethodDecl]) extends Tree {
    override def print(level: Int = 0): String = {
      val extend = parent map (" extends " + _.print()) getOrElse ""
      val vari = vars.map (indent(level + 1) + _.print(level + 1) + "\n") mkString ""
      val meti = methods.map (indent(level + 1) + _.print(level + 1) + "\n") mkString ""

      this.indent(level) + "class " + id.print() + extend + " {\n" + vari + meti + this.indent(level) + "}"
    }
  }
  case class VarDecl(tpe: TypeTree, id: Identifier) extends Tree {
    override def print(level: Int = 0): String = s"var ${id.print()} : ${tpe.print()};"
  }
  case class MethodDecl(
      retType: TypeTree,
      id: Identifier,
      args: List[Formal],
      vars: List[VarDecl],
      stats: List[StatTree],
      retExpr: ExprTree) extends Tree {
    override def print(level: Int = 0): String = {
      val arg = args map (_.print()) mkString ", "
      val vari = vars map ("\n" + indent(level + 1) + _.print(level + 1)) mkString ""
      val stmt = stats map ("\n" + indent(level + 1) + _.print(level + 1)) mkString ""
      "def " + id.print() + " ( " + arg + ") : " + retType.print() + " = {" +
        vari + stmt + "\n" +
        indent(level + 1) + "return " + retExpr.print() + ";\n" +
        indent(level) + "}"
    }
  }
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree {
    override def print(level: Int = 0): String = id.print() + " : " + tpe.print()
  }

  sealed trait TypeTree extends Tree
  case class IntArrayType() extends TypeTree {
    override def print(level: Int = 0): String = "Int[ ]"
  }
  case class IntType() extends TypeTree {
    override def print(level: Int = 0): String = "Int"
  }
  case class BooleanType() extends TypeTree {
    override def print(level: Int = 0): String = "Bool"
  }
  case class StringType() extends TypeTree {
    override def print(level: Int = 0): String = "String"
  }

  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree {
    override def print(level: Int = 0): String = {
      val statments = stats map ("\n" + indent(level + 1) + _.print(level + 1)) mkString ""
      "{" + statments + "\n" + indent(level) + "}"
    }
  }
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree {
    override def print(level: Int = 0): String = {
      val addElse = els map ("\n" + indent(level) + "else \n" + indent(level + 1) + _.print(level + 1)) getOrElse "";
      "if ( " + expr.print() + " )\n" + this.indent(level + 1) + thn.print(level + 1) + addElse
    }
  }
  case class While(expr: ExprTree, stat: StatTree) extends StatTree {
    override def print(level: Int = 0): String = {
      "while ( " + expr.print() + " )\n" + this.indent(level + 1) + stat.print(level + 1)
    }
  }
  case class Println(expr: ExprTree) extends StatTree {
    override def print(level: Int = 0): String = "println( " + expr.print() + " );"
  }
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree {
    override def print(level: Int = 0): String = id.print() + " = " + expr.print() + ";"
  }
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree {
    override def print(level: Int = 0): String = id.print() + "[ " + index.print() + " ] = " + expr.print() + ";"
  }

  sealed trait ExprTree extends Tree
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = lhs.print() + " && " + rhs.print()
  }
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = lhs.print() + " || " + rhs.print()
  }
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = lhs.print() + " + " + rhs.print()
  }
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = lhs.print() + " - " + rhs.print()
  }
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = lhs.print() + " * " + rhs.print()
  }
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = lhs.print() + " / " + rhs.print()
  }
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = lhs.print() + " < " + rhs.print()
  }
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = lhs.print() + " == " + rhs.print()
  }
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = arr.print() + " [ " + index.print() + " ]"
  }
  case class ArrayLength(arr: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = arr.print() + ".length"
  }
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree {
    override def print(level: Int = 0): String = {
      val arg = args map (_.print()) mkString ", "
      obj.print() + "." + meth.print() + " ( " + arg + " )"
    }
  }
  case class IntLit(value: Int) extends ExprTree {
    override def print(level: Int = 0): String = value.toString
  }
  case class StringLit(value: String) extends ExprTree {
    override def print(level: Int = 0): String = '"' + value + '"'
  }

  case class True() extends ExprTree {
    override def print(level: Int = 0): String = "true"
  }
  case class False() extends ExprTree {
    override def print(level: Int = 0): String = "false"
  }
  case class Identifier(value: String) extends TypeTree with ExprTree {
    override def print(level: Int = 0): String = value
  }

  case class This() extends ExprTree {
    override def print(level: Int = 0): String = "this"
  }
  case class NewIntArray(size: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = "new Int [ " + size.print() + " ] "
  }
  case class New(tpe: Identifier) extends ExprTree {
    override def print(level: Int = 0): String = "new " + tpe.print() + "()"
  }

  case class Not(expr: ExprTree) extends ExprTree {
    override def print(level: Int = 0): String = "!" + expr.print()
  }
}
