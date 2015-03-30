/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package analyzer

import utils._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def symbol: Option[S] = _sym
  }

  sealed abstract class Symbol extends Positioned {
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
    def lookupClass(n: String): Option[ClassSymbol] = ???
  }

  class ClassSymbol(val name: String, val members: Map[String, VariableSymbol]) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String,MethodSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = ???
    def lookupVar(n: String): Option[VariableSymbol] = ???
  }

  class MethodSymbol(
      val name: String,
      val classSymbol: ClassSymbol,
      val members: Map[String,VariableSymbol]
      ) extends Symbol {
    var params = Map[String,VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden : Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = ???
  }

  class VariableSymbol(val name: String) extends Symbol
}
