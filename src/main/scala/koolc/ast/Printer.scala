/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast

import Trees._
import analyzer.Symbols._

object Printer extends (Boolean => (Tree => String)) {
  def apply(showSymbols: Boolean = false): (Tree => String) = printTree(showSymbols)

  def indent(ident: Int): String = " " * ident

  def printTree(showSymbols: Boolean): (Tree => String) = {
    def print(t: Tree, ident: Int): String = t match {
      case Program(t1,t2) => {
        var tmp = ""
        t2.foreach {tmp += print(_,ident)}
        print(t1,ident+1) + tmp
      }

      case MainObject(id , stats) => {
        val statments = stats map { indent(ident+1) + print(_,ident+1) } mkString "\n"
        val mainMethod = indent(ident) + "def main() : Unit = {\n" + statments + "\n" +indent(ident)+"}"
        "object " + print(id,ident+1) + " {\n" + mainMethod + "\n}\n\n"
      }

      case ClassDecl(id, parent,vars,methods) => {
        val extend = parent map (" extends " + print(_,ident)) getOrElse ""
        val vari = vars map { indent(ident+1) + print(_,ident+1) }
        val meti = methods map { indent(ident+1) + print(_,ident+1) }

        val body = vari ++: meti mkString "\n"
        "class " + print(id,ident+1) + extend + " {\n" + body + "}\n\n"
      }

      case VarDecl(tpe, id) => {
        "var " + print(id,ident) + " : " + print(tpe,ident) + ";"
      }

      case MethodDecl(retType, id, args, vars, stats, retExpr) => {
        val arg = args map (print(_,ident)) mkString ", "
        val vari = vars map { indent(ident+1) + print(_,ident+1) }
        val stmt = stats map { indent(ident+1) + print(_,ident+1) }
        val ret = indent(ident+1) + "return " + print(retExpr,ident+1) + ";"
        val body = (vari ++: stmt ++: List(ret)) mkString "\n"
        "def " + print(id,ident) + " ( " + arg + " ) : " + print(retType,ident) + " = {\n" + body + "\n"+indent(ident)+"}\n"
      }

      case Formal(tpe, id) => print(id,ident) + " : " + print(tpe,ident)
      case IntArrayType()  => "Int[ ]"
      case IntType()       => "Int"
      case BooleanType()   => "Bool"
      case StringType()    => "String"

      case Block(stats) => {
        val body = stats map { indent(ident+1) + print(_,ident+1) } mkString "\n"
        "{\n" + body + "\n}"
      }

      case If(expr, thn, els) => {
        val addElse = els map { elseStatement =>
            indent(ident) + "\nelse\n" + indent(ident+1) + print(elseStatement,ident+1)
          } getOrElse "";

        "if ( " + print(expr,ident) + " )\n" +indent(ident+1)+ print(thn,ident+1) + addElse
      }

      case While(expr, stat) => {
        "while ( " + print(expr,ident) + " )\n" + print(stat,ident)
      }

      case Println(expr) => "println( " + print(expr,ident) + " );"
      case Assign(id, expr) => print(id,ident) + " = " + print(expr,ident) + ";"
      case ArrayAssign(id, index, expr) =>
        print(id,ident) + "[ " + print(index,ident) + " ] = " + print(expr,ident) + ";"

      case And(lhs, rhs: ExprTree)         => "(" + print(lhs,ident) + " && " + print(rhs,ident) +")"
      case Or(lhs, rhs: ExprTree)          => "(" + print(lhs,ident) + " || " + print(rhs,ident) +")"
      case Plus(lhs, rhs: ExprTree)        => "(" + print(lhs,ident) + " + " + print(rhs,ident) +")"
      case Minus(lhs, rhs: ExprTree)       => "(" + print(lhs,ident) + " - " + print(rhs,ident) +")"
      case Times(lhs, rhs: ExprTree)       => "(" + print(lhs,ident) + " * " + print(rhs,ident) +")"
      case Div(lhs, rhs: ExprTree)         => "(" + print(lhs,ident) + " / " + print(rhs,ident) +")"
      case LessThan(lhs, rhs: ExprTree)    => "(" + print(lhs,ident) + " < " + print(rhs,ident) +")"
      case Equals(lhs, rhs: ExprTree)      => "(" + print(lhs,ident) + " == " + print(rhs,ident) +")"
      case ArrayRead(arr, index: ExprTree) => print(arr,ident) + " [ " + print(index,ident) + " ]"
      case ArrayLength(arr)                => print(arr,ident) + ".length"

      case MethodCall(obj, meth, args) => {
          val arg = args map (print(_,ident)) mkString ", "
          print(obj,ident) + "." + print(meth,ident) + " ( " + arg + " )"
      }

      case IntLit(value)    => value.toString
      case StringLit(value) => '"' + value + '"'
      case True()           => "true"
      case False()          => "false"

      case NewIntArray(size) => "new Int [ " + print(size,ident+1) + " ]"
      case New(tpe)          => "new " + print(tpe,ident+1) + "()"
      case Not(expr)         => "!" + print(expr,ident+1)

      case t: This              => "this" + (if(showSymbols) t.symbolComment else "")
      case id@Identifier(value) => value + (if(showSymbols) id.symbolComment else "")
    }
    print(_, 0)
  }
}
