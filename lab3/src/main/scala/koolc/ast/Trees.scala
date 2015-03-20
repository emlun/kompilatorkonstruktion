package koolc
package ast

import utils._

object Trees {
  sealed trait Tree extends Positioned{
  def print(): String ={"\n>>>\nTODO " + toString + "\n<<<\n\n"}
  }

  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree{
    override def print(): String = {
      var classImpl: String = ""
      classes.foreach (classImpl += "\n" + _.print())
      main.print() + classImpl
      }
  }
  case class MainObject(id: Identifier, stats: List[StatTree]) extends Tree{
    override def print(): String = {
      var statments: String = ""
      stats.foreach (statments += "\n" + _.print())
      "object " + id.print() + " {\ndef main() : Unit = {"+statments+ "\n}\n}\n"}
  }
  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree{
    override def print(): String = {
      var extend = "";
      parent match{
        case Some(value) => extend = " extends " + value.print;
        case None        => extend = "";
      }
      var vari: String = ""
      vars.foreach (vari += _.print() + "\n")
      var meti: String = ""
      methods.foreach (meti += _.print()+ "\n")

      "class " + id.print() + extend + " {\n" + vari + meti + "\n}"
      }

  }
  case class VarDecl(tpe: TypeTree, id: Identifier) extends Tree{
    override def print(): String = {"var " + id.print() + " : " + tpe.print() + ";"}
  }
  case class MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree) extends Tree{
    override def print(): String = {
      var arg: String = ""
      // Pattern match
      args match {
        case first :: second :: rest  =>
          arg = first.print() + ", " + second.print()
          rest.foreach (arg += ", " + _.print())
        case first :: rest =>
          arg = first.print()
        case _ => arg = ""
      }
      var vari: String = ""
      vars.foreach (vari += _.print()+ "\n")
      var stmt: String = ""
      stats.foreach (stmt += _.print()+ "\n")
      "def " + id.print() + " ( " + arg + ") : " + retType.print + " = {\n" +
        vari + stmt + "return "+ retExpr.print + ";\n}"
      }
  }
  sealed case class Formal(tpe: TypeTree, id: Identifier) extends Tree{
    override def print(): String = {id.print +" : "+tpe.print}
  }

  sealed trait TypeTree extends Tree
  case class IntArrayType() extends TypeTree{
    override def print(): String = {"Int[ ]"}
  }
  case class IntType() extends TypeTree{
    override def print(): String = {"Int"}
  }
  case class BooleanType() extends TypeTree{
    override def print(): String = {"Bool"}
  }
  case class StringType() extends TypeTree{
    override def print(): String = {"String"}
  }

  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree{

    override def print(): String = {
      var statments: String = ""
      stats.foreach (statments += "\n" + _.print())
      "{" + statments + "\n}"
      }
  }
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree{
    var addElse = "";
    els match{
      case Some(value) => addElse = "\nelse \n" + value.print;
      case None        => addElse = "";
    }
    override def print(): String = {"if ( " + expr.print + " )\n" +thn.print + addElse}
  }
  case class While(expr: ExprTree, stat: StatTree) extends StatTree{
    override def print(): String = {"while ( " + expr.print + " )\n" +stat.print}
  }
  case class Println(expr: ExprTree) extends StatTree{
    override def print(): String = {"println( " + expr.print() + " );"}
  }
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree{
    override def print(): String = {id.print + " = " + expr.print + ";"}
  }
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree{
    override def print(): String = {id.print + "[ " + index.print + " ] = " + expr.print + ";"}
  }

  sealed trait ExprTree extends Tree
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree{
    override def print(): String = {lhs.print + " && " + rhs.print}
  }
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree{
    override def print(): String = {lhs.print + " || " + rhs.print}
  }
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree{
    override def print(): String = {lhs.print + " + " + rhs.print}
  }
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree{
    override def print(): String = {"(" + lhs.print + " - " + rhs.print + ")"}
  }
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree{
    override def print(): String = {lhs.print + " * " + rhs.print}
  }
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree{
    override def print(): String = {lhs.print + " / " + rhs.print}
  }
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree{
    override def print(): String = {lhs.print + " < " + rhs.print}
  }
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree{
    override def print(): String = {lhs.print + " == " + rhs.print}
  }
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree{
    override def print(): String = {arr.print()+" [ "+index.print+" ]"}
  }
  case class ArrayLength(arr: ExprTree) extends ExprTree{
    override def print(): String = {arr.print()+".lenght"}
  }
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree{
    override def print(): String = {
      var arg: String = ""
      // Pattern match
      args match {
        case first :: second :: rest  =>
          arg = first.print() + ", " + second.print()
          rest.foreach (arg += ", " + _.print())
        case first :: rest =>
          arg = first.print()
        case _ => arg = ""
      }
      obj.print + "." + meth.print + " ( " + arg + " )"
    }
  }
  case class IntLit(value: Int) extends ExprTree{
    override def print(): String = {value.toString}
  }
  case class StringLit(value: String) extends ExprTree{
    override def print(): String = {value}
  }

  case class True() extends ExprTree{
    override def print(): String = {"true"}
  }
  case class False() extends ExprTree{
    override def print(): String = {"false"}
  }
  case class Identifier(value: String) extends TypeTree with ExprTree{
    override def print(): String = {value}
  }

  case class This() extends ExprTree{
    override def print(): String = {"this"}
  }
  case class NewIntArray(size: ExprTree) extends ExprTree{
    override def print(): String = {"new Int [ " + size.print() + " ] "}
  }
  case class New(tpe: Identifier) extends ExprTree{
    override def print(): String = {"new " + tpe.print + "()"}
  }

  case class Not(expr: ExprTree) extends ExprTree{
    override def print(): String = {"!" + expr.print}
  }
}
