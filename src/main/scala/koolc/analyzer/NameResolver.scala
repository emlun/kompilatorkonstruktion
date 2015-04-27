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


object NameResolver {

  def run(ctx: Context)
         (program: Program, mainSymbol: ClassSymbol, classSymbols: List[ClassSymbol]): Option[Program] = {

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

      if(!(used contains varOrParam.symbol)) {
        ctx.reporter.warning(s"${kind} ${varOrParam.symbol.name} in ${ancestors} is never used.", varOrParam.symbol)
      }
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

        method.args ++ method.vars foreach warnIfUnused(usedVars, clazz, Some(method.symbol))
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

    def setSymbolReferencesOnClass(
        lookupType: (Identifier => Option[ClassSymbol]),
        mainSymbol: ClassSymbol,
        classDecl: ClassDecl)
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
        setSymbolReferences(lookupType, clazz, method.symbol.lookupVar _)(method)
      }
      classDecl.vars foreach warnIfUnused(usedVars.toSet, clazz)
    }


    val global = new GlobalScope(mainSymbol, classSymbols.map(clazz => (clazz.name, clazz)).toMap)

    program.main.stats foreach setSymbolReferences(lookupType(global, mainSymbol), mainSymbol, (_ => None))

    program.classes foreach { clazz =>
      setSymbolReferencesOnClass(lookupType(global, mainSymbol), mainSymbol, clazz)(clazz.symbol)
    }

    def checkMethods(clazz: ClassSymbol): Boolean = {
      clazz.parent match {
        case Some(parent) => {
          clazz.methods.foreach { case(methodName, methodSymbol) =>
            //println(method._1)
            parent.lookupMethod(methodName) map { pMethod =>
              if(methodSymbol.params.size == pMethod.params.size) {
                methodSymbol.overridden = Some(pMethod)
              } else {
                ctx.reporter.error(s"${methodName} overloads previous definition in ${parent.name} with a different number of parameters.", methodSymbol)
                ctx.reporter.info(s"${parent.name}.${methodName} declared here:", pMethod)
              }
            }
          }
          clazz.members.foreach{
            member => //println(method._1)
            parent.lookupVar(member._1) match {
              case Some(pMember) => {
                  ctx.reporter.error(s"${member._1}  test member declaration overrides previous declaration at ${pMember.position}.", member._2)
              }
              case None =>
            }
          }
          true
          }
        case None => true
      }
    }

    classSymbols.foreach {checkMethods(_)}

    def setTypeOnVarOrParam(tpe: TypeTree, symbol: VariableSymbol) = tpe match {
        case id: Identifier => lookupType(global, mainSymbol)(id) map { symbol.setType(_) }
        case _ => {}
      }

    program.classes foreach { clazz =>
      clazz.vars foreach { varpar => setTypeOnVarOrParam(varpar.tpe, varpar.symbol) }
      clazz.methods foreach { method =>
        method.args foreach { varpar => setTypeOnVarOrParam(varpar.tpe, varpar.symbol) }
        method.vars foreach { varpar => setTypeOnVarOrParam(varpar.tpe, varpar.symbol) }
      }
    }

    if(ctx.reporter.hasErrors) None
    else Some(program)
  }
}
