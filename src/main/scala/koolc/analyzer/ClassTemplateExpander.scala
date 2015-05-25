/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object ClassTemplateExpander {

  def run(ctx: Context)
         (program: Program, mainSymbol: ClassSymbol, classSymbols: List[ClassSymbol]): Option[Program] = {

    def getClassTemplateReferences(program: Program): List[Identifier] = {
      def getInType(tpe: TypeTree): List[Identifier] = tpe match {
          case Identifier(value, Nil)         => Nil
          case id@Identifier(value, template) => List(id)
          case _                              => Nil
        }

      def getInExpression(expression: ExprTree): List[Identifier] = expression match {
          case And(lhs, rhs)               => getInExpression(lhs) ++: getInExpression(rhs)
          case Or(lhs, rhs)                => getInExpression(lhs) ++: getInExpression(rhs)
          case Plus(lhs, rhs)              => getInExpression(lhs) ++: getInExpression(rhs)
          case Minus(lhs, rhs)             => getInExpression(lhs) ++: getInExpression(rhs)
          case Times(lhs, rhs)             => getInExpression(lhs) ++: getInExpression(rhs)
          case Div(lhs, rhs)               => getInExpression(lhs) ++: getInExpression(rhs)
          case LessThan(lhs, rhs)          => getInExpression(lhs) ++: getInExpression(rhs)
          case Equals(lhs, rhs)            => getInExpression(lhs) ++: getInExpression(rhs)
          case ArrayRead(arr, index)       => getInExpression(arr) ++: getInExpression(index)
          case ArrayLength(arr)            => getInExpression(arr)
          case MethodCall(obj, meth, args) => getInExpression(obj) ++: (args flatMap getInExpression _)
          case NewIntArray(size)           => getInExpression(size)
          case Not(expr)                   => getInExpression(expr)
          case New(tpe)                    => getInType(tpe)
          case IntLit(_) | StringLit(_)| True() | False() | Identifier(_,_) | This() => Nil
        }

      def getInStatement(statement: StatTree): List[Identifier] = statement match {
          case Block(stats)                 => stats flatMap getInStatement _
          case If(expr, thn, els)           => getInExpression(expr) ++: getInStatement(thn) ++:
                                               (els map getInStatement _ getOrElse Nil)
          case While(expr, stat)            => getInExpression(expr) ++: getInStatement(stat)
          case Println(expr)                => getInExpression(expr)
          case Assign(id, expr)             => getInExpression(expr)
          case ArrayAssign(id, index, expr) => getInExpression(index) ++: getInExpression(expr)
        }

      def getInMethod(method: MethodDecl): List[Identifier] = {
        val retTypeTemplate: List[Identifier] = getInType(method.retType)
        val argTemplates: List[Identifier] = method.args flatMap { arg => getInType(arg.tpe) }
        val varTemplates: List[Identifier] = method.vars flatMap { varDecl => getInType(varDecl.tpe) }
        val statementTemplates: List[Identifier] = method.stats flatMap getInStatement _
        val retExprTemplates: List[Identifier] = getInExpression(method.retExpr)

        retTypeTemplate ++: argTemplates ++: varTemplates ++: statementTemplates ++: retExprTemplates
      }

      def getInClass(clazz: ClassDecl): List[Identifier] = {
        val parentTemplate: Option[Identifier] = clazz.parent filter { ! _.template.isEmpty }
        val variableTypeTemplates: List[Identifier] = clazz.vars map { _.tpe } flatMap {
          case Identifier(value, Nil)         => None
          case id@Identifier(value, template) => Some(id)
          case _                              => None
        }
        val methodTypeTemplates: List[Identifier] = clazz.methods flatMap getInMethod _

        parentTemplate ++: variableTypeTemplates ++: methodTypeTemplates
      }

      (program.main.stats flatMap getInStatement _) ++:
        (program.classes filter { _.template.isEmpty } flatMap getInClass _)
    }

    val classTemplateReferences = getClassTemplateReferences(program).toSet
    println(classTemplateReferences)

    def expandClassTemplates(program: Program): Program = {
      program
    }
    val expandedProgram = expandClassTemplates(program)

    if(ctx.reporter.hasErrors) None
    else NameResolver.run(ctx)(expandedProgram, mainSymbol, classSymbols)
  }

}
