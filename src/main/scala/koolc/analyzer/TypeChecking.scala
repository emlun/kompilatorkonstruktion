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
        case MethodCall(obj, meth, args) => {
          println(meth)
          tcExpr(obj,anyObject)
        }

        case id@Identifier(value) => {
          id.symbol.tpe
        }

        case New(obj) => {
          tcExpr(obj,anyObject)
          obj.symbol.tpe
        }
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
        case If(expr, thn, els) => {
          tcExpr(expr,TBoolean)
        }

        case _ => println("tcStat not implemented >>> " + stat)
      }
    }

    prog match {
      case Some(p) => p.main.stats foreach { stat => tcStat(stat)}
      case None => ???

    }

    println("hej")

    prog
  }

}
