package koolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[ Option[Program], Option[Program]] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Option[Program]): Option[Program] = prog flatMap { program =>
    import ctx.reporter._

    def resolveMethodCall(call: MethodCall): Option[MethodSymbol] = {
      val objType = tcExpr(call.obj)

      objType match {
        case TObject(classSymbol) =>
          classSymbol.lookupMethod(call.meth.value) map { methodSymbol =>
            call.meth.setSymbol(methodSymbol)
            methodSymbol
          }
        case _ => {
          ctx.reporter.error(s"Type error: Expected class type, found: ${objType}", call.obj)
          None
        }
      }
    }

    def tcTypeTree (tTree: TypeTree): Type = {
      val tpe = tTree match {
        case IntArrayType() => TArray
        case IntType() => TInt
        case BooleanType() => TBoolean
        case StringType() => TString
        case id@Identifier(value,templateList) => {
          id.symbol.tpe
        }
      }
      tTree.setType(tpe)
      tpe
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs,rhs) => {
          val tlhs = tcExpr(lhs)
          tcExpr(rhs,tlhs)
          TBoolean
        }
        case Or(lhs,rhs) => {
          val tlhs = tcExpr(lhs)
          tcExpr(rhs,tlhs)
          TBoolean
        }
        case Plus(lhs,rhs) => {
          val tlhs = tcExpr(lhs,TInt,TString)
          val trhs = tcExpr(rhs,TInt, TString)
          if(tlhs == TInt && trhs == TInt) TInt
          else TString
        }
        case Minus(lhs,rhs) => {
          tcExpr(lhs,TInt)
          tcExpr(rhs,TInt)
          TInt
        }
        case Times(lhs,rhs) => {
          tcExpr(lhs,TInt)
          tcExpr(rhs,TInt)
          TInt
        }
        case Div(lhs,rhs) => {
          tcExpr(lhs,TInt)
          tcExpr(rhs,TInt)
          TInt
        }
        case LessThan(lhs,rhs) => {
          tcExpr(lhs,TInt)
          tcExpr(rhs,TInt)
          TBoolean
        }
        case Equals(lhs,rhs) => {
          val tlhs = tcExpr(lhs)
          tcExpr(rhs, tlhs match {
              case tObj: TObject => anyObject
              case _             => tlhs
            }
          )
          TBoolean
        }
        case ArrayRead(arr, index) => {
          tcExpr(arr,TArray)
          tcExpr(index,TInt)
          TInt
        }
        case ArrayLength(arr) => {
          tcExpr(arr,TArray)
          TInt
        }
        case call@MethodCall(obj, methId, args) => {
          val objType = tcExpr(obj,anyObject)
          resolveMethodCall(call) map { methodSymbol =>

            if(args.size == methodSymbol.argList.size) {
              args zip methodSymbol.argList foreach { case (arg, argDef) => tcExpr(arg, argDef.tpe) }
            } else if(args.size < methodSymbol.argList.size) {
              ctx.reporter.error(s"Too few parameters for method ${methId.value}", call)
            } else if(args.size > methodSymbol.argList.size) {
              ctx.reporter.error(s"Too many parameters for method ${methId.value}", call)
            }

            tcExpr(methodSymbol.decl.retExpr)
            tcTypeTree(methodSymbol.decl.retType)
          } getOrElse {
            ctx.reporter.error(s"Unknown method ${methId.value} in type ${objType}")
            TError
          }
        }
        case IntLit(value)=> TInt
        case StringLit(value)=> TString

        case True() => TBoolean
        case False() => TBoolean
        case id@Identifier(value,templateList) => {
          id.symbol.tpe
        }

        case id@This() => {
          id.symbol.tpe
        }
        case NewIntArray(size) => {
          tcExpr(size,TInt)
          TArray
        }
        case New(obj) => {
          tcExpr(obj,anyObject)
          obj.symbol.tpe
        }

        case Not(expr) => tcExpr(expr,TBoolean)

        case _ => {
          println("tcExpr not implemented >>> " + expr)
          TUntyped
        }
      }

      expr.setType(tpe)

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats) => {
          stats foreach {tcStat(_)}
        }
        case If(expr, thn, els) => {
          tcExpr(expr,TBoolean)
          tcStat(thn)
          els match {
            case Some(statTree) => tcStat(statTree)
            case None =>
          }
        }
        case While(expr, stat) => {
          tcExpr(expr,TBoolean)
          tcStat(stat)
        }
        case Println(expr) => {
          tcExpr(expr,TString,TInt,TBoolean)
        }
        case Assign(id, expr) => {
          tcExpr(expr, id.symbol.tpe)
        }
        case ArrayAssign(id, index, expr) => {
          tcExpr(id,TArray)
          tcExpr(index,TInt)
          tcExpr(expr,TInt)
        }
        case _ => println("tcStat not implemented >>> " + stat)
      }
    }

  ////
  def findMethodInExpr(expr: ExprTree): List[MethodCall] = {
    expr match {
        case And(lhs,rhs) => findMethodInExpr(lhs) ++ findMethodInExpr(rhs)
        case Or(lhs,rhs) => findMethodInExpr(lhs) ++  findMethodInExpr(rhs)
        case Plus(lhs,rhs) => findMethodInExpr(lhs) ++ findMethodInExpr(rhs)
        case Minus(lhs,rhs) => findMethodInExpr(lhs) ++ findMethodInExpr(rhs)
        case Times(lhs,rhs) => findMethodInExpr(lhs) ++ findMethodInExpr(rhs)
        case Div(lhs,rhs) => findMethodInExpr(lhs) ++ findMethodInExpr(rhs)
        case LessThan(lhs,rhs) => findMethodInExpr(lhs) ++ findMethodInExpr(rhs)
        case Equals(lhs,rhs) => findMethodInExpr(lhs) ++ findMethodInExpr(rhs)
        case ArrayRead(arr, index) => findMethodInExpr(arr) ++ findMethodInExpr(index)
        case ArrayLength(arr) => findMethodInExpr(arr)
        case NewIntArray(size) => findMethodInExpr(size)
        case New(obj) => findMethodInExpr(obj)
        case Not(expr) => findMethodInExpr(expr)

        case call@MethodCall(obj, methId, args) => {
          findMethodInExpr(obj) match {
            case Nil => {
              if(methId.template.isEmpty){
                (args flatMap findMethodInExpr _ )
              }else
                List(call)
              }
              case whatever => whatever
            }
          }
        case _ => Nil
      }
    }

    def findMethodInStatment(stat: StatTree): List[MethodCall] = {
      stat match {
        case Block(stats) => {
          stats flatMap {findMethodInStatment _ }
        }
        case If(expr, thn, els) => {
          findMethodInExpr(expr) ++
          findMethodInStatment(thn) ++
          {els match {
            case Some(statTree) => findMethodInStatment(statTree)
            case None => Nil
          }}
        }
        case While(expr, stat) => {
          findMethodInExpr(expr) ++
          findMethodInStatment(stat)
        }
        case Println(expr) => {
          findMethodInExpr(expr)
        }
        case Assign(id, expr) => {
          findMethodInExpr(expr)
        }
        case ArrayAssign(id, index, expr) => {
          findMethodInExpr(id) ++
          findMethodInExpr(index) ++
          findMethodInExpr(expr)
        }
        case _ => {println("findMethodInStatment not implemented >>> " + stat); Nil}
      }
    }

    def findMethodTemplateReferences(program: Program): List[MethodCall] = {
      (program.main.stats flatMap { stats => findMethodInStatment(stats) }) ++
      (program.classes flatMap { clazz =>
        clazz.methods flatMap { method =>
          {
            findMethodInExpr(method.retExpr) ++ (method.stats flatMap findMethodInStatment _ )
          }
         }
      })
    }

    def expandMethodTemplateReferences(program: Program, refs: List[MethodCall]): Program = {
      ???
    }

    val methodTemplateRefs = findMethodTemplateReferences(program)

    if(methodTemplateRefs.isEmpty) {
      program.main.stats foreach { stat => tcStat(stat)}
      program.classes foreach {
        clazz => clazz.methods foreach {
          method => {
            method.symbol.overridden map { overridden =>
              method.args zip overridden.decl.args foreach { case(overridingArg, overriddenArg) =>
                if(tcTypeTree(overridingArg.tpe) != tcTypeTree(overriddenArg.tpe)) {
                  ctx.reporter.error(
                    s"Formal type in overriding method ${method.id.value} does not match type in overridden method.",
                    overridingArg
                  )
                }
              }
              if(tcTypeTree(method.retType) != tcTypeTree(overridden.decl.retType)) {
                ctx.reporter.error(
                  s"Method ${method.id.value} overrides parent method with a dirrerent return type (${method.retType.name} and ${overridden.decl.retType.name})",
                  method.retType
                )
              }
            }
            method.stats foreach {
              tcStat(_)
            }
            tcExpr(method.retExpr, tcTypeTree(method.retType))
          }
        }
      }

      if(ctx.reporter.hasErrors) {
        None
      } else Some(program)
    } else {
      val newProgram = expandMethodTemplateReferences(program, methodTemplateRefs)
      run(ctx)(Some(newProgram))
    }
  }

}
