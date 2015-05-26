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
    println("NameAnalysis.run")

    def createVariableSymbol(varDecl: VarDecl): VariableSymbol = {
      val symbol = new VariableSymbol(varDecl.id.value, varDecl.tpe).setPos(varDecl.id)
      varDecl.setSymbol(symbol)
      varDecl.id.setSymbol(symbol)
      symbol
    }

    def createParameterSymbol(param: Formal): VariableSymbol = {
      val symbol = new VariableSymbol(param.id.value, param.tpe).setPos(param.id)
      param.setSymbol(symbol)
      param.id.setSymbol(symbol)
      symbol
    }

    def makeClassVariablesSymbolMap(clazz: ClassDecl): Map[String, VariableSymbol] =
      clazz.vars.foldLeft(Map[String, VariableSymbol]())((varSymbols, varDecl) => {
        val varSymbol = createVariableSymbol(varDecl)
        varSymbols.get(varDecl.id.value) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Member ${clazz.id.value}.${varDecl.id.value} declared multiple times", varSymbol);
            ctx.reporter.info(s"${clazz.id.value}.${varDecl.id.value} first declared here:", existingSymbol);
            varSymbols
          }
          case None => varSymbols + (varDecl.id.value -> varSymbol)
        }
      })

    def createMethodSymbol(classSymbol: ClassSymbol, method: MethodDecl): MethodSymbol = {
      val methodSymbol = new MethodSymbol(method.id.value, classSymbol,
        makeMethodVariablesSymbolMap(method),
        makeMethodArgList(method),
        method
      ).setPos(method.id)
      method.setSymbol(methodSymbol)
      method.id.setSymbol(methodSymbol)
      methodSymbol.params = makeMethodParameterSymbolsMap(methodSymbol)

      def detectShadowedParameter(param: VariableSymbol): Unit = {
        methodSymbol.members.get(param.name) map { varSymbol =>
          ctx.reporter.error(
            s"Variable ${varSymbol.name} in method ${classSymbol.name}.${methodSymbol.name} shadows parameter.",
            varSymbol
          )
          ctx.reporter.info(s"Parameter ${param.name} declared here:", param)
        }
      }
      methodSymbol.params.values map detectShadowedParameter _

      methodSymbol
    }

    def makeMethodSymbolsMap(clazz: ClassDecl, classSymbol: ClassSymbol): Map[String, MethodSymbol] =
      clazz.methods.foldLeft(Map[String, MethodSymbol]())((symbols, method) => {
        val symbol = createMethodSymbol(classSymbol, method)
        symbols.get(method.id.value) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Method ${method.id.value} declared multiple times", symbol);
            ctx.reporter.info(s"${method.id.value} first declared here:", existingSymbol);
            symbols
          }
          case None => symbols + (method.id.value -> symbol)
        }
      })

      def makeMethodArgList(method: MethodDecl): List[VariableSymbol] =
        method.args.map (argDecl => {
          createParameterSymbol(argDecl)
          })


    def makeMethodParameterSymbolsMap(methodSymbol: MethodSymbol): Map[String, VariableSymbol] =
      methodSymbol.argList.foldLeft(Map[String, VariableSymbol]())((symbols, param) => {
        symbols.get(param.name) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Parameter ${param.name} declared multiple times", param);
            ctx.reporter.info(s"${param.name} first declared here:", existingSymbol);
            symbols
          }
          case None => symbols + (param.name -> param)
        }
      })




    def makeMethodVariablesSymbolMap(method: MethodDecl): Map[String, VariableSymbol] =
      method.vars.foldLeft(Map[String, VariableSymbol]())((varSymbols, varDecl) => {
        val varSymbol = createVariableSymbol(varDecl)
        varSymbols.get(varDecl.id.value) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Variable ${varDecl.id.value} declared multiple times", varSymbol);
            ctx.reporter.info(s"${varDecl.id.value} first declared here:", existingSymbol);
            varSymbols
          }
          case None => varSymbols + (varDecl.id.value -> varSymbol)
        }
      })

    def createClassSymbol(clazz: ClassDecl): ClassSymbol = {
      val classSymbol = new ClassSymbol(clazz.id.value, makeClassVariablesSymbolMap(clazz), clazz).setPos(clazz.id)
      clazz.setSymbol(classSymbol)
      clazz.id.setSymbol(classSymbol)

      classSymbol.methods = makeMethodSymbolsMap(clazz, classSymbol)

      classSymbol
    }

    val mainSymbol = new ClassSymbol(program.main.id.value, Map.empty, ClassDecl(program.main.id, None, Nil, Nil,Nil)).setPos(program.main.id)
    program.main.setSymbol(mainSymbol)
    program.main.id.setSymbol(mainSymbol)

    val classSymbols = mainSymbol :: ( program.classes map createClassSymbol _ )

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
    else NameResolver.run(ctx)(program, mainSymbol, classSymbols)
  }
}
