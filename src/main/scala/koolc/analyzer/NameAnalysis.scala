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

    val mainSymbol = new ClassSymbol(program.main.id.value, Map.empty)
    program.main.setSymbol(mainSymbol)

    def createVariableSymbol(varDecl: VarDecl): VariableSymbol = {
      val symbol = new VariableSymbol(varDecl.id.value).setPos(varDecl)
      varDecl.setSymbol(symbol)
      symbol
    }

    def makeClassVariablesSymbolMap(clazz: ClassDecl): Map[String, VariableSymbol] =
      clazz.vars.foldLeft(Map[String, VariableSymbol]())((varSymbols, varDecl) =>
        varSymbols.get(varDecl.id.value) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Member ${clazz.id.value}.${varDecl.id.value} declared multiple times", varDecl);
            ctx.reporter.info(s"${clazz.id.value}.${varDecl.id.value} first declared here:", existingSymbol);
            varSymbols
          }
          case None => varSymbols + (varDecl.id.value -> createVariableSymbol(varDecl))
        }
      )

    def createMethodSymbol(classSymbol: ClassSymbol, method: MethodDecl): MethodSymbol = {
      val parameterSymbols = method.args map { parameter =>
        val symbol = new VariableSymbol(parameter.id.value)
        parameter.setSymbol(symbol)
        symbol
      }

      val methodSymbol = new MethodSymbol(method.id.value, classSymbol, makeMethodVariablesSymbolMap(method))
      method.setSymbol(methodSymbol)
      methodSymbol
    }

    def makeMethodSymbolsMap(clazz: ClassDecl, classSymbol: ClassSymbol): Map[String, MethodSymbol] =
      clazz.methods.foldLeft(Map[String, MethodSymbol]())((symbols, method) =>
        symbols.get(method.id.value) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Method ${method.id.value} declared multiple times", method);
            ctx.reporter.info(s"${method.id.value} first declared here:", existingSymbol);
            symbols
          }
          case None => symbols + (method.id.value -> createMethodSymbol(classSymbol, method))
        }
      )

    def makeMethodVariablesSymbolMap(method: MethodDecl): Map[String, VariableSymbol] =
      method.vars.foldLeft(Map[String, VariableSymbol]())((varSymbols, varDecl) =>
        varSymbols.get(varDecl.id.value) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Variable ${varDecl.id.value} declared multiple times", varDecl);
            ctx.reporter.info(s"${varDecl.id.value} first declared here:", existingSymbol);
            varSymbols
          }
          case None => varSymbols + (varDecl.id.value -> createVariableSymbol(varDecl))
        }
      )

    val classSymbols = mainSymbol :: {
      program.classes map { clazz =>
        val classSymbol = new ClassSymbol(clazz.id.value, makeClassVariablesSymbolMap(clazz))
        clazz.setSymbol(classSymbol)

        classSymbol.methods = makeMethodSymbolsMap(clazz, classSymbol)

        classSymbol
      }
    }

    val classSymbolsMap: Map[String, ClassSymbol] =
      classSymbols.foldLeft(Map[String, ClassSymbol]())((symbolsMap, classSymbol) =>
        symbolsMap.get(classSymbol.name) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Class ${classSymbol.name} declared multiple times", classSymbol);
            ctx.reporter.info(s"${classSymbol.name} first declared here:", existingSymbol);
            symbolsMap
          }
          case None => symbolsMap + (classSymbol.name -> classSymbol)
        }
      )

    if(ctx.reporter.hasErrors) None
    else Some(program)
  }
}
