/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package analyzer

import scala.collection.mutable.ListBuffer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Option[Program], Option[Program]] {

  def run(ctx: Context)(prog: Option[Program]): Option[Program] = prog flatMap { program =>

    def detectCyclicInheritance(baseSym: ClassSymbol)(ancestorSym: ClassSymbol): Option[Seq[String]] =
      if(ancestorSym == baseSym) {
        Some(ancestorSym.name :: Nil)
      } else {
        ancestorSym.parent flatMap detectCyclicInheritance(baseSym) map { ancestorSym.name +: _ }
      }

    def warnIfUnused(used: Set[VariableSymbol], clazz: ClassSymbol, method: Option[MethodSymbol] = None)
        (varOrParam: Symbolic[VariableSymbol] with Positioned) = {
      val kind = varOrParam match {
        case v: VarDecl => "Variable"
        case p: Formal  => "Parameter"
      }
      val ancestors = method map {
        methodSym => "Method " + clazz.name + "." + methodSym.name
      } getOrElse { "Class " + clazz.name }

      varOrParam.symbol map { symbol =>
        if(!(used contains symbol)) {
          ctx.reporter.warning(s"${kind} ${symbol.name} in ${ancestors} is never used.", symbol)
        }
      } orElse sys.error(s"${kind} has no symbol: ${varOrParam}")
    }

    def lookupType(global: GlobalScope, mainSymbol: ClassSymbol)(id: Identifier): Option[ClassSymbol] =
      global.lookupClass(id.value) map { classSymbol =>
        if(classSymbol == mainSymbol) {
          ctx.reporter.error(s"Main object cannot be used as type.", id);
        }
        classSymbol
      }

    def setSymbolReferences(
        lookupType: (Identifier => Option[ClassSymbol]),
        clazz: ClassSymbol,
        lookupVar: (String => Option[VariableSymbol])
        )(tree: Tree): Set[VariableSymbol] = {

      def setOnStatement(statement: StatTree): Set[VariableSymbol] = statement match {
        case Block(substats) => (substats flatMap setOnStatement _).toSet
        case If(expression, thn, els) => {
          setOnExpression(expression) ++
          setOnStatement(thn) ++
          (els map setOnStatement _ getOrElse Set.empty)
        }
        case While(expression, statement) => {
          setOnExpression(expression) ++
          setOnStatement(statement)
        }
        case Println(expression) => setOnExpression(expression)
        case Assign(id, expression) => {
          (lookupVar(id.value) map { symbol =>
            id.setSymbol(symbol)
            symbol
          } orElse {
            ctx.reporter.error(s"Assignment to undeclared identifier: ${id}", id)
            None
          }) ++:
          setOnExpression(expression)
        }
        case ArrayAssign(id, index, expression) => {
          (lookupVar(id.value) map { symbol =>
            id.setSymbol(symbol)
            symbol
          } orElse {
            ctx.reporter.error(s"Array assignment to undeclared identifier: ${id}", id)
            None
          }) ++:
          setOnExpression(index) ++:
          setOnExpression(expression)
        }
      }

      def setOnExpression(expression: ExprTree): Set[VariableSymbol] = expression match {
        case And(lhs, rhs)               => { setOnExpression(lhs) ++ setOnExpression(rhs) }
        case Or(lhs, rhs)                => { setOnExpression(lhs) ++ setOnExpression(rhs) }
        case Plus(lhs, rhs)              => { setOnExpression(lhs) ++ setOnExpression(rhs) }
        case Minus(lhs, rhs)             => { setOnExpression(lhs) ++ setOnExpression(rhs) }
        case Times(lhs, rhs)             => { setOnExpression(lhs) ++ setOnExpression(rhs) }
        case Div(lhs, rhs)               => { setOnExpression(lhs) ++ setOnExpression(rhs) }
        case LessThan(lhs, rhs)          => { setOnExpression(lhs) ++ setOnExpression(rhs) }
        case Equals(lhs, rhs)            => { setOnExpression(lhs) ++ setOnExpression(rhs) }
        case ArrayRead(arr, index)       => { setOnExpression(arr) ++ setOnExpression(index) }
        case ArrayLength(arr)            => setOnExpression(arr)
        case MethodCall(obj, meth, args) => {
          setOnExpression(obj) ++
          // Process method identifier in a later stage with type checking
          (args flatMap setOnExpression _)
        }
        case id: Identifier              => {
          // It can only be a variable if we end up here,
          // since we don't descend into MethodCall.meth or New.tpe
          lookupVar(id.value) map { symbol =>
            id.setSymbol(symbol)
            Set(symbol)
          } orElse {
            ctx.reporter.error(s"Reference to undeclared identifier: ${id.value}", id)
            None
          } getOrElse Set.empty
        }
        case ths: This                   => { ths.setSymbol(clazz); Set.empty }
        case NewIntArray(size)           => setOnExpression(size)
        case New(tpe)                    => {
          lookupType(tpe) map { tpe.setSymbol(_) } orElse {
            ctx.reporter.error(s"Reference to undeclared type: ${tpe.value}", tpe)
            None
          }
          Set.empty
        }
        case Not(expr)                   => setOnExpression(expr)
        case _                           => Set.empty
      }

      def setOnType(tpe: TypeTree): Unit = tpe match {
          case id: Identifier => lookupType(id) map { id.setSymbol(_) } orElse {
              ctx.reporter.error(s"Reference to undeclared type: ${id.value}", tpe)
              None
            }
          case _ => {}
        }

      def setOnMethod(method: MethodDecl): Set[VariableSymbol] = {
        setOnType(method.retType)
        method.args foreach { param   => setOnType(param.tpe)   }
        method.vars foreach { varDecl => setOnType(varDecl.tpe) }

        val usedVars = (method.stats flatMap setOnStatement _) ++:
          setOnExpression(method.retExpr)

        method.args ++ method.vars foreach warnIfUnused(usedVars, clazz, method.symbol)
        usedVars
      }

      tree match {
        case statement: StatTree         => setOnStatement(statement)
        case expr: ExprTree              => setOnExpression(expr)
        case VarDecl(tpe: Identifier, _) => { setOnType(tpe); Set.empty }
        case method: MethodDecl          => setOnMethod(method)
        case _                           => Set.empty
      }
    }

    def setSymbolReferencesOnClass(lookupType: (Identifier => Option[ClassSymbol]), mainSymbol: ClassSymbol, classDecl: ClassDecl)
        (clazz: ClassSymbol): Unit = {
      classDecl.parent map { parentId =>
        lookupType(parentId) orElse {
          ctx.reporter.error(s"Class ${classDecl.id.value} extends undeclared type: ${parentId.value}", parentId)
          None
        } filter { parentSym =>
          if(parentSym == mainSymbol) {
            ctx.reporter.error(s"Class ${classDecl.id.value} must not extend main object", parentId)
            false
          } else true
        } map { parentSym =>
          clazz.parent = Some(parentSym)
          parentId.setSymbol(parentSym)

          detectCyclicInheritance(clazz)(parentSym) map { ancestorNames  =>
            ctx.reporter.error(
              "Cyclic inheritance detected: " + ((clazz.name +: ancestorNames) mkString " <: "), clazz
            )
          }
        }
      }

      classDecl.vars foreach setSymbolReferences(lookupType, clazz, clazz.lookupVar _)

      val usedVars = classDecl.methods flatMap { method =>
        method.symbol map { methodSymbol =>
          setSymbolReferences(lookupType, clazz, methodSymbol.lookupVar _)(method)
        } orElse {
          sys.error(s"Method no longer has a symbol: ${method}")
          None
        } getOrElse Set.empty
      }
      classDecl.vars foreach warnIfUnused(usedVars.toSet, clazz)
    }

    val mainSymbol = new ClassSymbol(program.main.id.value, Map.empty).setPos(program.main.id)
    program.main.setSymbol(mainSymbol)
    program.main.id.setSymbol(mainSymbol)

    def createVariableSymbol(varDecl: VarDecl): VariableSymbol = {
      val symbol = new VariableSymbol(varDecl.id.value).setPos(varDecl.id)
      varDecl.setSymbol(symbol)
      varDecl.id.setSymbol(symbol)
      symbol
    }

    def createParameterSymbol(param: Formal): VariableSymbol = {
      val symbol = new VariableSymbol(param.id.value).setPos(param.id)
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
        makeMethodParameterSymbolsMap(method)
      ).setPos(method.id)
      method.setSymbol(methodSymbol)
      method.id.setSymbol(methodSymbol)

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

    def makeMethodParameterSymbolsMap(method: MethodDecl): Map[String, VariableSymbol] =
      method.args.foldLeft(Map[String, VariableSymbol]())((symbols, param) => {
        val symbol = createParameterSymbol(param)
        symbols.get(param.id.value) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Parameter ${param.id.value} declared multiple times", symbol);
            ctx.reporter.info(s"${param.id.value} first declared here:", existingSymbol);
            symbols
          }
          case None => symbols + (param.id.value -> symbol)
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
      val classSymbol = new ClassSymbol(clazz.id.value, makeClassVariablesSymbolMap(clazz)).setPos(clazz.id)
      clazz.setSymbol(classSymbol)
      clazz.id.setSymbol(classSymbol)

      classSymbol.methods = makeMethodSymbolsMap(clazz, classSymbol)

      classSymbol
    }

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

    val global = new GlobalScope(mainSymbol, classSymbols.map(clazz => (clazz.name, clazz)).toMap)

    program.main.stats foreach setSymbolReferences(lookupType(global, mainSymbol), mainSymbol, (_ => None))

    program.classes foreach { clazz =>
      clazz.symbol orElse {
        sys.error(s"Class no longer has a symbol: ${clazz}")
        None
      } map setSymbolReferencesOnClass(lookupType(global, mainSymbol), mainSymbol, clazz)
    }

    if(ctx.reporter.hasErrors) None
    else Some(program)
  }
}
