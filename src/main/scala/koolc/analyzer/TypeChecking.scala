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

    def expandMethodTemplateId(id: Identifier): Identifier =
      Identifier(id.value + "$" + (id.template map { _.name2 } mkString ","), Nil).setPos(id)

    def expandMethodTemplateReferences(program: Program, refs: List[MethodCall]): Program = {
      refs.foldLeft(program) { (program: Program, ref: MethodCall) =>
        //println("Expand method call " + ref)

        resolveMethodCall(ref) map { sym =>
          val methodClassSymbol = sym.classSymbol
          val methodClass = sym.classSymbol.decl
          val expandedId = expandMethodTemplateId(ref.meth)

          val typeMap: Map[String, TypeTree] = (sym.decl.template map { _.value } zip ref.meth.template).toMap

          def expandTypeTree(tpe: TypeTree): TypeTree = tpe match {
              case id@Identifier(value, template) => typeMap.get(value) match {
                case Some(replacement@Identifier(templateValue, templateValueTemplate)) =>
                  Identifier(templateValue, templateValueTemplate map expandTypeTree _).setPos(id)
                case Some(templateValue) => templateValue
                case None                => Identifier(value, template map expandTypeTree _).setPos(id)
              }
              case _                     => tpe
            }

          def expandInExpr(expr: ExprTree): ExprTree = expr match {
              case And(lhs, rhs)               => And(expandInExpr(lhs), expandInExpr(rhs)).setPos(expr)
              case Or(lhs, rhs)                => Or(expandInExpr(lhs), expandInExpr(rhs)).setPos(expr)
              case Plus(lhs, rhs)              => Plus(expandInExpr(lhs), expandInExpr(rhs)).setPos(expr)
              case Minus(lhs, rhs)             => Minus(expandInExpr(lhs), expandInExpr(rhs)).setPos(expr)
              case Times(lhs, rhs)             => Times(expandInExpr(lhs), expandInExpr(rhs)).setPos(expr)
              case Div(lhs, rhs)               => Div(expandInExpr(lhs), expandInExpr(rhs)).setPos(expr)
              case LessThan(lhs, rhs)          => LessThan(expandInExpr(lhs), expandInExpr(rhs)).setPos(expr)
              case Equals(lhs, rhs)            => Equals(expandInExpr(lhs), expandInExpr(rhs)).setPos(expr)
              case ArrayRead(arr, index)       => ArrayRead(expandInExpr(arr), expandInExpr(index)).setPos(expr)
              case ArrayLength(arr)            => ArrayLength(expandInExpr(arr)).setPos(expr)
              case MethodCall(obj, meth, args) => MethodCall(expandInExpr(obj), meth, (args map expandInExpr _)).setPos(expr)
              case NewIntArray(size)           => NewIntArray(expandInExpr(size)).setPos(expr)
              case Not(expr)                   => Not(expandInExpr(expr)).setPos(expr)
              case New(tpe)                    => expandTypeTree(tpe) match {
                case id: Identifier => New(id).setPos(expr)
                case other => {
                  ctx.reporter.error("Expected class type, found " + other, tpe)
                  IntLit(0).setPos(tpe)
                }
              }
              case This()                      => This()
              case id: Identifier              => id.copy()
              case whatever                    => whatever
            }

          def expandTemplateReferencesInStatement(statement: StatTree): StatTree = statement match {
              case Block(stats)                 => Block(stats map expandTemplateReferencesInStatement _).setPos(statement)
              case If(expr, thn, els)           => If(
                  expandInExpr(expr),
                  expandTemplateReferencesInStatement(thn),
                  els map expandTemplateReferencesInStatement _
                ).setPos(statement)
              case While(expr, stat)            => While(
                  expandInExpr(expr),
                  expandTemplateReferencesInStatement(stat)
                ).setPos(statement)
              case Println(expr)                => Println(expandInExpr(expr)).setPos(statement)
              case Assign(id, expr)             => Assign(id.copy(), expandInExpr(expr)).setPos(statement)
              case ArrayAssign(id, index, expr) => ArrayAssign(
                  id.copy(),
                  expandInExpr(index),
                  expandInExpr(expr)
                ).setPos(statement)
            }

          val expandedProgram = methodClassSymbol.lookupMethod(expandedId.value) map { sym =>
            program
          } getOrElse {
            val newMethodDecl = MethodDecl(
              retType = expandTypeTree(sym.decl.retType),
              id = expandedId,
              args = sym.decl.args map { arg => Formal(expandTypeTree(arg.tpe), arg.id.copy()).setPos(arg) },
              vars = sym.decl.vars map { varDecl => VarDecl(expandTypeTree(varDecl.tpe), varDecl.id.copy()).setPos(varDecl) },
              stats = sym.decl.stats map expandTemplateReferencesInStatement _,
              retExpr = expandInExpr(sym.decl.retExpr),
              template = Nil
            )
            val newMethodSymbol = new MethodSymbol(
              name = expandedId.value,
              classSymbol = sym.classSymbol,
              members = sym.members,
              argList = sym.argList,
              decl = newMethodDecl
            )
            newMethodDecl.setSymbol(newMethodSymbol)

            val expandedProgram: Program = Program(
              program.main,
              program.classes map { clazz =>
                if(clazz.id.value == methodClass.id.value) {
                  val newClass = clazz.copy(methods = newMethodDecl +: clazz.methods).setSymbol(sym.classSymbol)
                  newClass.symbol.methods = newClass.symbol.methods + (expandedId.value -> newMethodDecl.symbol)
                  newClass
                } else {
                  clazz
                }
              }
            )

            println("Expanded program:")
            println(koolc.ast.Printer.printTree(true)(expandedProgram))

            expandedProgram
          }

          val newCall = ref.copy(meth = expandedId.copy())

          def replaceInExpr(expr: ExprTree): ExprTree = {
            expr match {
              case And(lhs, rhs)               => And(replaceInExpr(lhs), replaceInExpr(rhs)).setPos(expr)
              case Or(lhs, rhs)                => Or(replaceInExpr(lhs), replaceInExpr(rhs)).setPos(expr)
              case Plus(lhs, rhs)              => Plus(replaceInExpr(lhs), replaceInExpr(rhs)).setPos(expr)
              case Minus(lhs, rhs)             => Minus(replaceInExpr(lhs), replaceInExpr(rhs)).setPos(expr)
              case Times(lhs, rhs)             => Times(replaceInExpr(lhs), replaceInExpr(rhs)).setPos(expr)
              case Div(lhs, rhs)               => Div(replaceInExpr(lhs), replaceInExpr(rhs)).setPos(expr)
              case LessThan(lhs, rhs)          => LessThan(replaceInExpr(lhs), replaceInExpr(rhs)).setPos(expr)
              case Equals(lhs, rhs)            => Equals(replaceInExpr(lhs), replaceInExpr(rhs)).setPos(expr)
              case ArrayRead(arr, index)       => ArrayRead(replaceInExpr(arr), replaceInExpr(index)).setPos(expr)
              case ArrayLength(arr)            => ArrayLength(replaceInExpr(arr)).setPos(expr)
              case call@MethodCall(obj, meth, args) => {
                println("Check identifier " + meth + " against reference " + ref)
                if(meth == ref.meth) {
                    println("Replacing reference " + call)
                    MethodCall(
                    replaceInExpr(obj),
                    expandedId.copy().setPos(meth),
                    (args map replaceInExpr _)
                  ).setPos(expr)
                } else {
                  call.copy(obj = replaceInExpr(obj), args = args map replaceInExpr _).setPos(call)
                }
              }
              case NewIntArray(size)           => NewIntArray(replaceInExpr(size)).setPos(expr)
              case Not(expr)                   => Not(replaceInExpr(expr)).setPos(expr)
              case whatever                    => whatever
            }
          }

          def replaceTemplatesInStatement(statement: StatTree): StatTree = statement match {
              case Block(stats)                 => Block(stats map replaceTemplatesInStatement _).setPos(statement)
              case If(expr, thn, els)           => If(
                  replaceInExpr(expr),
                  replaceTemplatesInStatement(thn),
                  els map replaceTemplatesInStatement _
                ).setPos(statement)
              case While(expr, stat)            => While(
                  replaceInExpr(expr),
                  replaceTemplatesInStatement(stat)
                ).setPos(statement)
              case Println(expr)                => Println(replaceInExpr(expr)).setPos(statement)
              case Assign(id, expr)             => Assign(id, replaceInExpr(expr)).setPos(statement)
              case ArrayAssign(id, index, expr) => ArrayAssign(
                  id,
                  replaceInExpr(index),
                  replaceInExpr(expr)
                ).setPos(statement)
            }


          val replacedProgram: Program = Program(
            expandedProgram.main.copy(stats = expandedProgram.main.stats map replaceTemplatesInStatement _),
            expandedProgram.classes map { clazz =>
              clazz.copy(methods = clazz.methods map { method =>
                method.copy(
                  stats = method.stats map replaceTemplatesInStatement _,
                  retExpr = replaceInExpr(method.retExpr)
                )
              })
            }
          )

          replacedProgram
        } getOrElse {
          ctx.reporter.error(s"Template method not found: ${ref.meth.value}", ref)
          program
        }
      }
    }

    val methodTemplateRefs = findMethodTemplateReferences(program)

    if(methodTemplateRefs.isEmpty) {
      // Remove template classes and methods
      val reducedProgram = Program(program.main,
        program.classes filter { _.template.isEmpty } map { clazz =>
          clazz.copy(methods = clazz.methods filter { _.template.isEmpty }).setPos(clazz).setSymbol(clazz.symbol)
        }
      )

      reducedProgram.main.stats foreach { stat => tcStat(stat)}
      reducedProgram.classes foreach {
        clazz => clazz.methods filter { _.template.isEmpty } foreach {
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
      } else Some(reducedProgram)
    } else {
      val newProgram = expandMethodTemplateReferences(program, methodTemplateRefs)
      run(ctx)(ClassTemplateExpander.run(ctx)(Some(newProgram)))
    }
  }

}
