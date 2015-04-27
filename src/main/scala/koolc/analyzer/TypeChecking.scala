package koolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[ Option[Program], Option[Program]] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Option[Program]): Option[Program] = {
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
      tTree match {
        case IntArrayType() => TArray
        case IntType() => TInt
        case BooleanType() => TBoolean
        case StringType() => TString
        case id@Identifier(value) => {
          id.symbol.tpe
        }

      }
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      // TODO: Compute type for each kind of expression
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
          tcExpr(rhs,tlhs)
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
          resolveMethodCall(call) flatMap { methodSymbol =>

            if(args.size == methodSymbol.argList.size) {
              args zip methodSymbol.argList foreach { case (arg, argDef) => tcExpr(arg, argDef.tpe) }
            } else if(args.size < methodSymbol.argList.size) {
              ctx.reporter.error(s"Too few parameters for method ${methId.value}", call)
            } else if(args.size > methodSymbol.argList.size) {
              ctx.reporter.error(s"Too many parameters for method ${methId.value}", call)
            }

            methodSymbol.classSymbol.decl.methods.find { _.symbol == methodSymbol } map { decl =>
              tcExpr(decl.retExpr)
            }
          } getOrElse {
            ctx.reporter.error(s"Unknown method ${methId.value} in type ${objType}")
            TError
          }
        }
        case IntLit(value)=> TInt
        case StringLit(value)=> TString

        case True() => TBoolean
        case False() => TBoolean
        case id@Identifier(value) => {
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

    prog match {
      case Some(p) => {
        p.main.stats foreach { stat => tcStat(stat)}
        p.classes foreach {
          clazz => clazz.methods foreach {
            method => {
              method.stats foreach {
                //println(_)
                tcStat(_)
              }
              tcExpr(method.retExpr, tcTypeTree(method.retType))
            }
          }
        }
      }
      case None => ???

    }


    prog
  }

}
