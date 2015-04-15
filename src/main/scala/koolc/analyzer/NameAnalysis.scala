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

    def setSymbolReferences(
        lookupType: (Identifier => Option[ClassSymbol]),
        clazz: ClassSymbol,
        lookupVar: (String => Option[VariableSymbol]),
        tree: Tree
        ): Unit = {

      def setOnStatement(statement: StatTree): Unit = statement match {
        case Block(substats) => substats foreach setOnStatement _
        case If(expression, thn, els) => {
          setOnExpression(expression)
          setOnStatement(thn)
          els foreach setOnStatement _
        }
        case While(expression, statement) => {
          setOnExpression(expression)
          setOnStatement(statement)
        }
        case Println(expression) => setOnExpression(expression)
        case Assign(id, expression) => {
          lookupVar(id.value) orElse {
            ctx.reporter.error(s"Assignment to undeclared identifier: ${id}", id)
            None
          } map { symbol => id.setSymbol(symbol) }
          setOnExpression(expression)
        }
        case ArrayAssign(id, index, expression) => {
          lookupVar(id.value) orElse {
            ctx.reporter.error(s"Array assignment to undeclared identifier: ${id}", id)
            None
          } map { symbol => id.setSymbol(symbol) }
          setOnExpression(index)
          setOnExpression(expression)
        }
      }

      def setOnExpression(expression: ExprTree): Unit = expression match {
        case And(lhs, rhs)               => { setOnExpression(lhs); setOnExpression(rhs) }
        case Or(lhs, rhs)                => { setOnExpression(lhs); setOnExpression(rhs) }
        case Plus(lhs, rhs)              => { setOnExpression(lhs); setOnExpression(rhs) }
        case Minus(lhs, rhs)             => { setOnExpression(lhs); setOnExpression(rhs) }
        case Times(lhs, rhs)             => { setOnExpression(lhs); setOnExpression(rhs) }
        case Div(lhs, rhs)               => { setOnExpression(lhs); setOnExpression(rhs) }
        case LessThan(lhs, rhs)          => { setOnExpression(lhs); setOnExpression(rhs) }
        case Equals(lhs, rhs)            => { setOnExpression(lhs); setOnExpression(rhs) }
        case ArrayRead(arr, index)       => { setOnExpression(arr); setOnExpression(index) }
        case ArrayLength(arr)            => setOnExpression(arr)
        case MethodCall(obj, meth, args) => {
          setOnExpression(obj)
          // Process method identifier in a later stage with type checking
          args foreach setOnExpression _
        }
        case id: Identifier              => {
          // It can only be a variable if we end up here,
          // since we don't descend into MethodCall.meth or New.tpe
          lookupVar(id.value) orElse {
            ctx.reporter.error(s"Reference to undeclared identifier: ${id.value}", id)
            None
          } map { symbol => id.setSymbol(symbol) }
        }
        case ths: This                   => ths.setSymbol(clazz)
        case NewIntArray(size)           => setOnExpression(size)
        case New(tpe)                    => {
          lookupType(tpe) orElse {
            ctx.reporter.error(s"Reference to undeclared type: ${tpe.value}", tpe)
            None
          } map { symbol => tpe.setSymbol(symbol) }
        }
        case Not(expr)                   => setOnExpression(expr)
        case _                           => {}
      }

      def setOnType(tpe: Identifier): Unit =
          lookupType(tpe) orElse {
            ctx.reporter.error(s"Reference to undeclared type: ${tpe.value}", tpe)
            None
          } map { symbol => tpe.setSymbol(symbol) }

      tree match {
        case statement: StatTree         => setOnStatement(statement)
        case expr: ExprTree              => setOnExpression(expr)
        case VarDecl(tpe: Identifier, _) => setOnType(tpe)
        case Formal(tpe: Identifier, _)  => setOnType(tpe)
        case _                           => {}
      }
    }

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints

    val mainSymbol = new ClassSymbol(program.main.id.value, Map.empty).setPos(program.main)
    program.main.setSymbol(mainSymbol)
    program.main.id.setSymbol(mainSymbol)

    def createVariableSymbol(varDecl: VarDecl): VariableSymbol = {
      val symbol = new VariableSymbol(varDecl.id.value).setPos(varDecl)
      varDecl.setSymbol(symbol)
      varDecl.id.setSymbol(symbol)
      symbol
    }

    def createParameterSymbol(param: Formal): VariableSymbol = {
      val symbol = new VariableSymbol(param.id.value).setPos(param)
      param.setSymbol(symbol)
      param.id.setSymbol(symbol)
      symbol
    }

    def makeClassVariablesSymbolMap(clazz: ClassDecl): Map[String, VariableSymbol] =
      clazz.vars.foldLeft(Map[String, VariableSymbol]())((varSymbols, varDecl) => {
        val varSymbol = createVariableSymbol(varDecl)
        varSymbols.get(varDecl.id.value) match {
          case Some(existingSymbol) => {
            ctx.reporter.error(s"Member ${clazz.id.value}.${varDecl.id.value} declared multiple times", varDecl);
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
      ).setPos(method)
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
            ctx.reporter.error(s"Method ${method.id.value} declared multiple times", method);
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
            ctx.reporter.error(s"Parameter ${param.id.value} declared multiple times", param);
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
            ctx.reporter.error(s"Variable ${varDecl.id.value} declared multiple times", varDecl);
            ctx.reporter.info(s"${varDecl.id.value} first declared here:", existingSymbol);
            varSymbols
          }
          case None => varSymbols + (varDecl.id.value -> varSymbol)
        }
      })

    def createClassSymbol(clazz: ClassDecl): ClassSymbol = {
      val classSymbol = new ClassSymbol(clazz.id.value, makeClassVariablesSymbolMap(clazz)).setPos(clazz)
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
    def lookupType(id: Identifier): Option[ClassSymbol] =
      global.lookupClass(id.value) map { classSymbol =>
        if(classSymbol == mainSymbol) {
          ctx.reporter.error(s"Main object cannot be used as type.", id);
        }
        classSymbol
      }

    def warnIfUnused(used: Set[VariableSymbol], clazz: ClassSymbol, method: Option[MethodSymbol] = None)
        (varOrParam: Symbolic[VariableSymbol] with Positioned) = {
      val (kind, pos) = varOrParam match {
        case v: VarDecl => ("Variable", v.id)
        case p: Formal  => ("Parameter", p)
      }
      val ancestors = method map {
        methodSym => "Method " + clazz.name + "." + methodSym.name
      } getOrElse { "Class " + clazz.name }

      varOrParam.symbol map { symbol =>
        if(!(used contains symbol)) {
          ctx.reporter.warning(s"${kind} ${symbol.name} in ${ancestors} is never used.", pos)
        }
      } orElse sys.error(s"${kind} has no symbol: ${varOrParam}")
    }

    def setSymbolReferencesInClass(clazz: ClassDecl)(classSymbol: ClassSymbol): Unit = {
      var usedVarsForClass: Set[VariableSymbol] = Set.empty

      def setSymbolReferencesInMethod(method: MethodDecl): Unit = {
        method.symbol orElse {
          sys.error(s"Method no longer has a symbol: ${method}")
          None
        } map { methodSymbol =>
          var usedVars: Set[VariableSymbol] = Set.empty
          def lookupVarAndRecordLookup(methodSymbol: MethodSymbol)(name: String): Option[VariableSymbol] = {
            val varSymbol = methodSymbol.lookupVar(name)
            varSymbol map { varSymbol =>
              usedVars += varSymbol
              usedVarsForClass += varSymbol
            }
            varSymbol
          }

          method.retType match {
            case id: Identifier => lookupType(id) orElse {
              ctx.reporter.error(
                s"Method ${method.id.value} in class ${clazz.id.value} returns undeclared type: ${id.value}", id
              )
              None
            } map id.setSymbol _
            case _              => {}
          }
          method.args foreach { param =>
            setSymbolReferences(lookupType, classSymbol, methodSymbol.lookupVar _, param)
          }
          method.vars foreach { varDecl =>
            setSymbolReferences(lookupType, classSymbol, methodSymbol.lookupVar _, varDecl)
          }
          method.stats foreach { statement =>
            setSymbolReferences(lookupType, classSymbol, lookupVarAndRecordLookup(methodSymbol), statement)
          }
          setSymbolReferences(lookupType, classSymbol, lookupVarAndRecordLookup(methodSymbol), method.retExpr)

          method.args foreach warnIfUnused(usedVars, classSymbol, Some(methodSymbol))
          method.vars foreach warnIfUnused(usedVars, classSymbol, Some(methodSymbol))
        }
      }

      clazz.parent map { parentId =>
        global.lookupClass(parentId.value) orElse {
          ctx.reporter.error(s"Class ${clazz.id.value} extends undeclared type: ${parentId.value}", parentId)
          None
        } filter { parentSym =>
          if(parentSym == mainSymbol) {
            ctx.reporter.error(s"Class ${clazz.id.value} must not extend main object", parentId)
            false
          } else true
        } map { parentSym =>
          classSymbol.parent = Some(parentSym)
          parentId.setSymbol(parentSym)

          def detectCyclicInheritance(ancestorSym: ClassSymbol): Option[Seq[String]] =
            if(ancestorSym.name == classSymbol.name) {
              Some(ancestorSym.name :: Nil)
            } else {
              ancestorSym.parent flatMap detectCyclicInheritance _ map { ancestorSym.name +: _ }
            }

          detectCyclicInheritance(parentSym) map { classSymbol.name +: _ } map { chain  =>
            ctx.reporter.error("Cyclic inheritance detected: " + (chain.toList mkString " <: "), clazz)
          }
        }
      }

      clazz.vars foreach { varDecl =>
        setSymbolReferences(lookupType, classSymbol, classSymbol.lookupVar _, varDecl)
      }

      clazz.methods foreach setSymbolReferencesInMethod _

      clazz.vars foreach warnIfUnused(usedVarsForClass, classSymbol)
    }

    program.main.stats foreach { statement =>
      setSymbolReferences(lookupType, mainSymbol, (_ => None), statement)
    }
    program.classes foreach { clazz =>
      clazz.symbol orElse {
        sys.error(s"Class no longer has a symbol: ${clazz}")
        None
      } map setSymbolReferencesInClass(clazz)
    }

    if(ctx.reporter.hasErrors) None
    else Some(program)
  }
}
