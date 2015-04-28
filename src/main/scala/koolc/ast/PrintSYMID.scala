/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast

import Trees._

object PrintSYMID extends (Tree => String) {
  def apply(t: Tree): String = print(t)

  def printClass(clazz: ClassDecl): String  =
    {
        val extend = clazz.parent map ( "Some(" + print(_) +")" ) getOrElse "None"
        val vari = clazz.vars map {  print(_) } mkString ", "
        val meti = clazz.methods map {  print(_) } mkString ", "
        "ClassDecl" +clazz.symbolComment+"("+print(clazz.id) +","+ extend + ",List(" + vari + "),List(" +meti+ "))"
    }
  def printMain(main: MainObject): String =
    {
      val statments = main.stats map {  print(_) } mkString ", "
      "MainObject" +main.symbolComment+ "(" + print(main.id) + ",List(" + statments + "))"
    }
  def printVar(vari: VarDecl): String =
    {
      "VarDecl"+vari.symbolComment+"(" + print(vari.tpe) +","+ print(vari.id) +")"
    }

  def printMethod(meth: MethodDecl): String =
  {
    val arg = "List(" + (meth.args map (print(_)) mkString ", ") + ")"
    val vari = "List(" + (meth.vars map (print(_))mkString ", ") + ")"
    val stmt = "List(" + (meth.stats map (print(_))mkString ", ")+ ")"
    "MethodDecl"+meth.symbolComment+"(" + print(meth.retType) +","+ print(meth.id) +
    "," + arg +  "," + vari +  ","+ stmt +","+print(meth.retExpr)+")"
  }

  def printFormal(form: Formal): String =
  {
    "Formal"+form.symbolComment+"("+print(form.tpe) + "," + print(form.id)+")"
  }

  def print(t: Tree): String ={
    t match {
      case Program(main, classes) =>{
        val claslist = "List(" + (classes map {  print(_) } mkString ", ")+")"
        "Program(" + print(main) +","+ claslist +")"
    }
    case id:MainObject => printMain(id)
    case id:ClassDecl => printClass(id)
    case id:MethodDecl => printMethod(id)
    case id:VarDecl => printVar(id)

    case id:Formal => printFormal(id)

    case Block(stats) => {
      val body = stats map {  print(_) } mkString ", "
      "Block(List(" +body+ "))"
    }

    case If(expr, thn, els) => {
      val addElse = els map { elseStatement =>
           "Some(" +  print(elseStatement) +")"
        } getOrElse "None";

      "If(" + print(expr) + "," + print(thn) +","+ addElse+")"
    }

    case While(expr, stat) => {
      "While(" + print(expr) + "," + print(stat) +")"
    }

    case Println(expr) => "Println(" + print(expr) + ")"
    case Assign(id, expr) => "Assign("+print(id) +","+print(expr)+")"
    case ArrayAssign(id, index, expr) =>
      "ArrayAssign(" +print(id) + "," + print(index) + "," + print(expr) + ")"

    case And(lhs, rhs: ExprTree) => "And(" + print(lhs) + "," + print(rhs) +")"
    case Or(lhs, rhs: ExprTree) => "Or(" + print(lhs) + "," + print(rhs) +")"
    case Plus(lhs, rhs: ExprTree) => "Plus(" + print(lhs) + "," + print(rhs) +")"
    case Minus(lhs, rhs: ExprTree) => "Minus(" + print(lhs) + "," + print(rhs) +")"
    case Times(lhs, rhs: ExprTree) => "Times(" + print(lhs) + "," + print(rhs) +")"
    case Div(lhs, rhs: ExprTree) => "Div(" + print(lhs) + "," + print(rhs) +")"
    case LessThan(lhs, rhs: ExprTree) => "LessThan(" + print(lhs) + "," + print(rhs) +")"
    case Equals(lhs, rhs: ExprTree) => "Equals(" + print(lhs) + "," + print(rhs) +")"

    case ArrayRead(arr, index: ExprTree) => "ArrayRead("+ print(arr) + "," + print(index) + ")"
    case ArrayLength(arr) => "ArrayLength(" +print(arr) + ")"

    case MethodCall(obj, meth, args) => {
        val arg = args map (print(_)) mkString ","
        "MethodCall(" + print(obj) + "," + print(meth) + ",List(" + arg + "))"
    }

    case IntLit(value) =>  "IntLit(" + value + ")"
    case StringLit(value) => "StringLit(" +value + ")"

    case True() =>  t.getClass.getSimpleName+"()"
    case False() =>  t.getClass.getSimpleName+"()"
    case IntArrayType() => t.getClass.getSimpleName+"()"
    case IntType() => t.getClass.getSimpleName+"()"
    case BooleanType() => t.getClass.getSimpleName+"()"
    case StringType() => t.getClass.getSimpleName+"()"


    case NewIntArray(size) =>  "NewIntArray(" + print(size) + ")"
    case New(tpe) =>  "New(" + print(tpe) + ")"
    case Not(expr) =>  "Not(" + print(expr) + ")"

    case id:This => "This" + id.symbolComment + "()"
    case id:Identifier => "Identifier" + id.symbolComment + "("+id.value+")"

    case _ => " >>> " + t.getClass.getSimpleName + "<<<"
    }
  }
}
