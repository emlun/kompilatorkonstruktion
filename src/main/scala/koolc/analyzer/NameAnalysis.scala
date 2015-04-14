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
        global: GlobalScope,
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
            ctx.reporter.error(s"Reference to undeclared identifier: ${id}", id)
            None
          } map { symbol => id.setSymbol(symbol) }
        }
        case ths: This                   => ths.setSymbol(clazz)
        case NewIntArray(size)           => setOnExpression(size)
        case New(tpe)                    => {
          global.lookupClass(tpe.value) orElse {
            ctx.reporter.error(s"Reference to undeclared type: ${tpe}", tpe)
            None
          } map { symbol => tpe.setSymbol(symbol) }
        }
        case Not(expr)                   => setOnExpression(expr)
        case _                           => {}
      }

      def setOnType(tpe: Identifier): Unit =
          global.lookupClass(tpe.value) orElse {
            ctx.reporter.error(s"Reference to undeclared type: ${tpe}", tpe)
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

    program.main.stats foreach { statement =>
      setSymbolReferences(global, mainSymbol, (_ => None), statement)
    }
    program.classes foreach { clazz =>
      clazz.symbol orElse {
        sys.error(s"Class no longer has a symbol: ${clazz}")
        None
      } map { classSymbol =>

        clazz.parent map { parentId =>
          global.lookupClass(parentId.value) orElse {
            ctx.reporter.error(s"Class ${clazz.id} extends undeclared type: ${parentId}", parentId)
            None
          } map { parentSym =>
            classSymbol.parent = Some(parentSym)
            parentId.setSymbol(parentSym)

            // Check for cyclic inheritance
            var chain = new ListBuffer[String]()
            chain += classSymbol.name
            var ancestor: Option[ClassSymbol] = Some(parentSym)
            while(ancestor.isDefined) {
              chain += ancestor.get.name
              if(ancestor.get.name == classSymbol.name) {
                ctx.reporter.error( "Cyclic inheritance detected: " + (chain.toList mkString " <: "), clazz)
                return None
              }
              ancestor = ancestor flatMap { _.parent }
            }
          }
        }

        clazz.vars foreach { varDecl =>
          setSymbolReferences(global, classSymbol, classSymbol.lookupVar _, varDecl)
        }

        clazz.methods foreach { method =>
          method.symbol orElse {
            sys.error(s"Method no longer has a symbol: ${method}")
            None
          } map { methodSymbol =>
            method.retType match {
              case id: Identifier => global.lookupClass(id.value) orElse {
                ctx.reporter.error(
                  s"Method ${method.id} in class ${clazz.id} returns undeclared type: ${id}", id
                )
                None
              } map id.setSymbol _
              case _              => {}
            }
            method.args foreach { param =>
              setSymbolReferences(global, classSymbol, methodSymbol.lookupVar _, param)
            }
            method.vars foreach { varDecl =>
              setSymbolReferences(global, classSymbol, methodSymbol.lookupVar _, varDecl)
            }
            method.stats foreach { statement =>
              setSymbolReferences(global, classSymbol, methodSymbol.lookupVar _, statement)
            }
            setSymbolReferences(global, classSymbol, methodSymbol.lookupVar _, method.retExpr)
          }
        }
      }
    }

    if(ctx.reporter.hasErrors) None
    else Some(program)
  }
}
