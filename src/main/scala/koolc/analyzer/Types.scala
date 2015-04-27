package koolc
package analyzer

import ast.Trees._
import Symbols._

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TUnresolved extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case Types.anyObject => true
      case _               => false
    }
    override def toString = "[unresolved class type]"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _        => false
    }
    override def toString = "bool"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _    => false
    }
    override def toString = "int"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _       => false
    }
    override def toString = "string"
  }

  case object TArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TArray => true
      case _      => false
    }
    override def toString = "array"
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case `anyObject` => true
      case TObject(cs) => cs == classSymbol || (classSymbol.parent match {
        case Some(parent) => TObject(parent).isSubTypeOf(tpe)
        case None => false
      })
      case _ => false
    }
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = TObject(new ClassSymbol("Object", Map.empty, ClassDecl(Identifier("Object"), None, Nil, Nil)))
}
