/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package analyzer

import utils._
import ast.Trees._
import Types._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def symbol: S = _sym getOrElse sys.error("Accessing undefined symbol.")
  }

  sealed abstract class Symbol extends Positioned with Typed{
    val id: Int = ID.next
    val name: String

    def tpe: Type

    override def equals(that: Any) = that match {
      case that: Symbol => this.id == that.id
      case _            => false
    }
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

  class ClassSymbol(val name: String, val members: Map[String, VariableSymbol], val decl: ClassDecl) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String,MethodSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = methods.get(n) orElse {
        parent flatMap ( _.lookupMethod(n) )
      }
    def lookupVar(n: String): Option[VariableSymbol] = members.get(n) orElse {
        parent flatMap ( _.lookupVar(n) )
      }

    val tpe: Type = TObject(this)
    def pureMethods: Map[String, MethodSymbol] = methods filter { case (_, sym) => sym.decl.template.isEmpty }
  }

  class MethodSymbol(
      val name: String,
      val classSymbol: ClassSymbol,
      val members: Map[String,VariableSymbol],
      val argList: List[VariableSymbol],
      val decl: MethodDecl
      ) extends Symbol {
    var params = Map[String,VariableSymbol]()
    var overridden : Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = {
      params.get(n) orElse { members.get(n) } orElse { classSymbol.lookupVar(n) }
    }

    val tpe: Type = TUntyped
  }

  class VariableSymbol(val name: String, val tpeTree: TypeTree) extends Symbol {
    private var _tpe: Type = tpeTree match {
      case BooleanType()  => TBoolean
      case IntType()      => TInt
      case StringType()   => TString
      case IntArrayType() => TArray
      case id: Identifier => TUnresolved
    }

    def setType(tpe: ClassSymbol): VariableSymbol = {
      _tpe = TObject(tpe)
      this
    }
    def tpe: Type = _tpe
  }

}
