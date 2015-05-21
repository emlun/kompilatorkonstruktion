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
          case Identifier(value, typeParams) => ???
          case _ => Nil
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

      def getInClass(clazz: ClassDecl): List[Identifier] = clazz match {
          case _ => ???
        }

      (program.main.stats flatMap getInStatement _) ++: (program.classes flatMap getInClass _)
    }

    val classTemplateReferences = getClassTemplateReferences(program)

    def expandClassTemplates(program: Program): Program = {
      program
    }
    val expandedProgram = expandClassTemplates(program)

    if(ctx.reporter.hasErrors) None
    else NameResolver.run(ctx)(expandedProgram, mainSymbol, classSymbols)
  }

}
