/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package analyzer

import utils._
import Types._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def symbol: Option[S] = _sym

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }

  }

  sealed abstract class Symbol extends Positioned with Typed{
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope(
      val mainClass: ClassSymbol,
      val classes: Map[String, ClassSymbol]
      ) {
    def lookupClass(n: String): Option[ClassSymbol] = n match {
      case mainClass.name => Some(mainClass)
      case _              => classes get n
    }
  }

  class ClassSymbol(val name: String, val members: Map[String, VariableSymbol]) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String,MethodSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = methods.get(n) orElse {
        parent flatMap ( _.lookupMethod(n) )
      }
    def lookupVar(n: String): Option[VariableSymbol] = members.get(n) orElse {
        parent flatMap ( _.lookupVar(n) )
      }
  }

  class MethodSymbol(
      val name: String,
      val classSymbol: ClassSymbol,
      val members: Map[String,VariableSymbol],
      val params: Map[String,VariableSymbol]
      ) extends Symbol {
    var argList: List[VariableSymbol] = Nil
    var overridden : Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = {
      params.get(n) orElse { members.get(n) } orElse { classSymbol.lookupVar(n) }
    }
  }

  class VariableSymbol(val name: String) extends Symbol
}
