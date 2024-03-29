package koolc
package analyzer

import ast.Trees._
import ast.TreeTraverser

import Symbols._
import Types._
import utils._
import Debug.debug

import scala.util.Try

object TypeChecking extends Pipeline[ Option[Program], Option[Program]] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Option[Program]): Option[Program] = prog flatMap { program =>
    debug("TypeChecking.run")
    debug("Program:")
    debug(koolc.ast.Printer.printTree(true)(program))

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
    def findMethodInExpr(expr: ExprTree): List[MethodCall] =
      TreeTraverser.collect(expr) {
        case call@MethodCall(obj, methId, args) => {
          findMethodInExpr(obj) match {
            case Nil => {
              Try(resolveMethodCall(call)) getOrElse None map { sym =>
                if(sym.decl.template.size != methId.template.size) {
                  ctx.reporter.error(s"Wrong number of type parameters for method template ${sym.name} (expected ${sym.decl.template.size}, found ${methId.template.size}).", methId)
                }
              }
              if(methId.template.isEmpty) {
                (args flatMap findMethodInExpr _ )
              } else List(call)
            }
            case whatever => whatever
          }
        }
      }

    def findMethodTemplateReferences(program: Program): List[MethodCall] =
      TreeTraverser.collect(program, descendIntoTemplates = false) {
        case expr: ExprTree => findMethodInExpr(expr)
      }

    def expandMethodTemplateReferences(program: Program, refs: List[MethodCall]): Program = {
      refs.foldLeft(program) { (program: Program, ref: MethodCall) =>
        resolveMethodCall(ref) map { sym =>
          val methodClassSymbol = sym.classSymbol
          val methodClass = sym.classSymbol.decl
          val methodClassName = methodClass.id.value
          val expandedId = ref.meth.expandTemplateName

          val typeMap: Map[String, TypeTree] = (sym.decl.template map { _.value } zip ref.meth.template).toMap

          val expandedProgram = methodClassSymbol.lookupMethod(expandedId.value) map { sym =>
            program
          } getOrElse {
            debug("Expanding method " + ref.meth.value + " with typeMap:")
            debug(typeMap)

            val newMethodDecl = TreeTraverser.transform(sym.decl) {
              case method: MethodDecl             => method.copy(id = expandedId, template = Nil).setPos(method)
              case ths: This                      => This().setPos(ths)
              case id@Identifier(value, template) => typeMap.get(value) map {
                  case Identifier(typeMapValue, _) => Identifier(typeMapValue, template).setPos(id)
                  case other                       => other
                } getOrElse id
              case expr@New(tpe)                  => tpe match {
                case id: Identifier => New(id).setPos(expr)
                case other          => {
                  ctx.reporter.error("Expected class type, found " + other, tpe)
                  IntLit(0).setPos(tpe)
                }
              }
            }

            val newMethodSymbol = new MethodSymbol(
              name = expandedId.value,
              classSymbol = sym.classSymbol,
              members = sym.members,
              argList = sym.argList,
              decl = newMethodDecl
            )
            newMethodDecl.setSymbol(newMethodSymbol)

            val expandedProgram: Program =
              TreeTraverser.transform(program) {
                case clazz@ClassDecl(Identifier(`methodClassName`, _), _, _, _, _) => {
                  val newClass = clazz.copy(
                      methods = newMethodDecl +: clazz.methods
                    ).setPos(clazz).setSymbol(sym.classSymbol)
                  newClass.symbol.methods = newClass.symbol.methods + (expandedId.value -> newMethodDecl.symbol)
                  newClass
                }
              }

            debug("Expanded program:")
            debug(koolc.ast.Printer.printTree(true)(expandedProgram))

            expandedProgram
          }

          def replaceInExpr(expr: ExprTree): ExprTree =
            TreeTraverser.transform(expr, {
              case call: MethodCall => false
            }) {
              case call@MethodCall(obj, meth, args) => {
                Try(resolveMethodCall(call)) getOrElse None flatMap { callSym: MethodSymbol =>
                  if(meth == ref.meth && sym == callSym) {
                    Some(MethodCall(
                      replaceInExpr(obj),
                      expandedId.copy().setPos(meth),
                      (args map replaceInExpr _)
                    ).setPos(expr))
                  } else {
                    None
                  }
                } getOrElse {
                  MethodCall(
                    obj = replaceInExpr(obj),
                    meth = meth.copy().setPos(meth),
                    args = args map replaceInExpr _
                  ).setPos(call)
                }
              }
            }

          val replacedProgram: Program =
            TreeTraverser.transform(expandedProgram, {
              case call: MethodCall => false
            }) {
              case call: MethodCall => replaceInExpr(call)
            }

          debug("Replaced program:")
          debug(koolc.ast.Printer.printTree(true)(replacedProgram))

          replacedProgram
        } getOrElse {
          ctx.reporter.error(s"Template method not found: ${ref.meth.value}", ref)
          program
        }
      }
    }

    program.classes foreach { clazz =>
      clazz.methods filter { ! _.template.isEmpty } foreach { method =>
        method.template.foldLeft[Set[Identifier]](
          clazz.template.toSet ++ program.classes.map(clazz => Identifier(clazz.id.value, Nil).setPos(clazz.id)).toSet
        ) { (allIds, paramId) =>
          debug("paramId: " + paramId)
          allIds find { _ == paramId } map { existingId =>
            ctx.reporter.error(s"Template parameter name collision: '${paramId.value}'", paramId)
            ctx.reporter.info(s"Name '${paramId.value}' first defined here:", existingId)
            allIds
          } getOrElse {
            allIds + paramId
          }
        }
      }
    }

    val methodTemplateRefs = findMethodTemplateReferences(program)

    if(ctx.reporter.hasErrors) {
      return None
    }

    if(methodTemplateRefs.isEmpty) {
      debug()
      debug("Nothing to expand - removing template methods!")
      debug()

      // Remove template classes and methods
      val reducedProgram = Program(program.main,
        program.classes filter { _.template.isEmpty } map { clazz =>
          clazz.copy(methods = clazz.methods filter { _.template.isEmpty }).setPos(clazz).setSymbol(clazz.symbol)
        }
      )

      debug()
      debug("Commencing type checks!")
      debug()
      debug("Program:")
      debug()
      debug(koolc.ast.Printer.printTree(true)(reducedProgram))

      reducedProgram.main.stats foreach { stat => tcStat(stat)}
      reducedProgram.classes foreach {
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

      debug("Finished program:")
      debug(koolc.ast.Printer.printTree(true)(reducedProgram))
      if(ctx.reporter.hasErrors) {
        None
      } else Some(reducedProgram)
    } else {
      val newProgram = expandMethodTemplateReferences(program, methodTemplateRefs)
      debug()
      debug("Things were expanded - returning to class template expander!")
      debug()
      run(ctx)(ClassTemplateExpander.run(ctx)(Some(newProgram)))
    }
  }

}
