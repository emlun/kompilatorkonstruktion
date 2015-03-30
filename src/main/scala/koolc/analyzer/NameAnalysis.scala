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

    program.main.setSymbol(new ClassSymbol(program.main.id.value))

    def createVariableSymbol(varDecl: VarDecl): VariableSymbol = {
      val symbol = new VariableSymbol(varDecl.id.value).setPos(varDecl)
      varDecl.setSymbol(symbol)
      symbol
    }

    val classSymbols = program.classes map { clazz =>
      val classSymbol = new ClassSymbol(clazz.id.value)
      clazz.setSymbol(classSymbol)

      val memberVariableSymbols = clazz.vars map { varDecl =>
        val symbol = new VariableSymbol(varDecl.id.value)
        varDecl.setSymbol(symbol)
        symbol
      }

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
