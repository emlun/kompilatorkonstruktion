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

object NameAnalysis extends Pipeline[Option[Program], Option[Program]] {

  def run(ctx: Context)(prog: Option[Program]): Option[Program] = prog flatMap { program =>

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints

    program.main.setSymbol(new ClassSymbol(program.main.id.value, Map.empty))

    def createVariableSymbol(varDecl: VarDecl): VariableSymbol = {
      val symbol = new VariableSymbol(varDecl.id.value).setPos(varDecl)
      varDecl.setSymbol(symbol)
      symbol
    }

    val classSymbols = program.classes map { clazz =>
      val memberVarSymbols =
        clazz.vars.foldLeft(Map[String, VariableSymbol]())((varSymbols, varDecl) => {
          varSymbols.get(varDecl.id.value) match {
            case Some(existingSymbol) => {
              ctx.reporter.error(s"Member ${clazz.id.value}.${varDecl.id.value} declared multiple times", varDecl);
              ctx.reporter.info(s"${clazz.id.value}.${varDecl.id.value} first declared here:", existingSymbol);
              varSymbols
            }
            case None => varSymbols + (varDecl.id.value -> createVariableSymbol(varDecl))
          }
        }
      )

      val classSymbol = new ClassSymbol(clazz.id.value, memberVarSymbols)
      clazz.setSymbol(classSymbol)

      val methodSymbols = clazz.methods map { method =>
        val methodSymbol = new MethodSymbol(method.id.value, classSymbol)
        method.setSymbol(methodSymbol)

        val parameterSymbols = method.args map { parameter =>
          val symbol = new VariableSymbol(parameter.id.value)
          parameter.setSymbol(symbol)
          symbol
        }

        val methodVariableSymbols = method.vars map { varDecl =>
          val symbol = new VariableSymbol(varDecl.id.value)
          varDecl.setSymbol(symbol)
          symbol
        }

        methodSymbol
      }

      classSymbol
    }

    if(ctx.reporter.hasErrors) None
    else Some(program)
  }
}
