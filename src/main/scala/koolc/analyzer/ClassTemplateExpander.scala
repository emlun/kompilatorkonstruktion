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

object ClassTemplateExpander {

  def run(ctx: Context)
         (program: Program, mainSymbol: ClassSymbol, classSymbols: List[ClassSymbol], recurse: Boolean = true): Option[Program] = {

    def expandClassId(classId: Identifier, template: List[TypeTree]): Identifier =
      Identifier(classId.value + "$" + (template map { _.name } mkString ","), Nil)

    def getClassTemplateReferences(program: Program): List[Identifier] = {
      def getInType(tpe: TypeTree): List[Identifier] = tpe match {
          case Identifier(value, Nil)         => Nil
          case id@Identifier(value, template) => List(id)
          case _                              => Nil
        }

      def getInExpression(expression: ExprTree): List[Identifier] = expression match {
          case And(lhs, rhs)               => getInExpression(lhs) ++: getInExpression(rhs)
          case Or(lhs, rhs)                => getInExpression(lhs) ++: getInExpression(rhs)
          case Plus(lhs, rhs)              => getInExpression(lhs) ++: getInExpression(rhs)
          case Minus(lhs, rhs)             => getInExpression(lhs) ++: getInExpression(rhs)
          case Times(lhs, rhs)             => getInExpression(lhs) ++: getInExpression(rhs)
          case Div(lhs, rhs)               => getInExpression(lhs) ++: getInExpression(rhs)
          case LessThan(lhs, rhs)          => getInExpression(lhs) ++: getInExpression(rhs)
          case Equals(lhs, rhs)            => getInExpression(lhs) ++: getInExpression(rhs)
          case ArrayRead(arr, index)       => getInExpression(arr) ++: getInExpression(index)
          case ArrayLength(arr)            => getInExpression(arr)
          case MethodCall(obj, meth, args) => getInExpression(obj) ++: (args flatMap getInExpression _)
          case NewIntArray(size)           => getInExpression(size)
          case Not(expr)                   => getInExpression(expr)
          case New(tpe)                    => getInType(tpe)
          case IntLit(_) | StringLit(_)| True() | False() | Identifier(_,_) | This() => Nil
        }

      def getInStatement(statement: StatTree): List[Identifier] = statement match {
          case Block(stats)                 => stats flatMap getInStatement _
          case If(expr, thn, els)           => getInExpression(expr) ++: getInStatement(thn) ++:
                                               (els map getInStatement _ getOrElse Nil)
          case While(expr, stat)            => getInExpression(expr) ++: getInStatement(stat)
          case Println(expr)                => getInExpression(expr)
          case Assign(id, expr)             => getInExpression(expr)
          case ArrayAssign(id, index, expr) => getInExpression(index) ++: getInExpression(expr)
        }

      def getInMethod(method: MethodDecl): List[Identifier] = {
        val retTypeTemplate: List[Identifier] = getInType(method.retType)
        val argTemplates: List[Identifier] = method.args flatMap { arg => getInType(arg.tpe) }
        val varTemplates: List[Identifier] = method.vars flatMap { varDecl => getInType(varDecl.tpe) }
        val statementTemplates: List[Identifier] = method.stats flatMap getInStatement _
        val retExprTemplates: List[Identifier] = getInExpression(method.retExpr)

        retTypeTemplate ++: argTemplates ++: varTemplates ++: statementTemplates ++: retExprTemplates
      }

      def getInClass(clazz: ClassDecl): List[Identifier] = {
        val parentTemplate: Option[Identifier] = clazz.parent filter { ! _.template.isEmpty }
        val variableTypeTemplates: List[Identifier] = clazz.vars map { _.tpe } flatMap {
          case Identifier(value, Nil)         => None
          case id@Identifier(value, template) => Some(id)
          case _                              => None
        }
        val methodTypeTemplates: List[Identifier] = clazz.methods flatMap getInMethod _

        parentTemplate ++: variableTypeTemplates ++: methodTypeTemplates
      }

      (program.main.stats flatMap getInStatement _) ++:
        (program.classes filter { _.template.isEmpty } flatMap getInClass _)
    }

    val classTemplateReferences = getClassTemplateReferences(program).toSet
    println("Template class references:")
    println(classTemplateReferences)

    def expandClassTemplate(program: Program)(reference: Identifier): Program = {
      val newProgram: Program = reference.template.foldLeft(program){ (program, ref) => ref match {
          case Identifier(_, Nil)         => program
          case id@Identifier(_, template) => expandClassTemplate(program)(id)
          case _                          => program
        }
      }

      val types = reference.template
      val newClass: Option[ClassDecl] = newProgram.classes find { _.id.value == reference.value } flatMap { clazz =>
        println("Type " + reference + " matched to " + clazz.id)

        if(clazz.template.size != types.size) {
          ctx.reporter.error(
            s"Wrong number of type parameters for class ${clazz.id.value} (expected ${clazz.template.size}, got ${types.size}: ${types})")
          None
        } else {
          val typeMap: Map[String, TypeTree] = (clazz.template map { _.value } zip types).toMap

          def expandTypeTree(tpe: TypeTree): TypeTree = {
            println(s"expandTypeTree(${tpe})")
            tpe match {
              case id@Identifier(value, template) => typeMap.get(value) match {
                case Some(Identifier(templateValue, templateValueTemplate)) =>
                  Identifier(templateValue, template map expandTypeTree _).setPos(id)
                case Some(templateValue) => templateValue
                case None                => Identifier(value, template map expandTypeTree _).setPos(id)
              }
              case _                     => tpe
            }
          }

          def expandInExpr(expr: ExprTree): ExprTree = {
            println(s"expandInExpr(${expr})")
            expr match {
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
              case New(tpe)                    => New(expandTypeTree(tpe).asInstanceOf[Identifier]).setPos(expr)
              case whatever                    => whatever
            }
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
              case Assign(id, expr)             => Assign(id, expandInExpr(expr)).setPos(statement)
              case ArrayAssign(id, index, expr) => ArrayAssign(
                  id,
                  expandInExpr(index),
                  expandInExpr(expr)
                ).setPos(statement)
            }

          def expandTemplateReferencesInMethod(method: MethodDecl): MethodDecl = {
            MethodDecl(
              retType = expandTypeTree(method.retType),
              id = method.id,
              args = method.args map { arg => Formal(expandTypeTree(arg.tpe), arg.id).setPos(arg) },
              vars = method.vars map { varDecl => VarDecl(expandTypeTree(varDecl.tpe), varDecl.id).setPos(varDecl) },
              stats = method.stats map expandTemplateReferencesInStatement _,
              retExpr = expandInExpr(method.retExpr),
              template = method.template).setPos(method)
          }

          println("Type map: " + typeMap)
          println("Expanded name: " + expandClassId(clazz.id, types).value)

          val newClassId = expandClassId(clazz.id, types)
          newProgram.classes find { clazz => clazz.id.value == newClassId.value } match {
            case Some(_) => None
            case None => {
              println("Class is not already expanded")
              val newDecl = ClassDecl(
                id = newClassId,
                parent = clazz.parent map { parent => expandTypeTree(parent).asInstanceOf[Identifier] },
                vars = clazz.vars map { varDecl => VarDecl(expandTypeTree(varDecl.tpe), varDecl.id).setPos(varDecl) },
                methods = clazz.methods map expandTemplateReferencesInMethod _,
                template = Nil).setPos(clazz)
              Some(newDecl)
            }
          }
        }
      }

      val newClasses = newProgram.classes ++ newClass
      Program(newProgram.main, newClasses)
    }

    def replaceTemplatesInProgram(program: Program, typeMap: Map[Identifier, Identifier]): Program = {
      println("replaceTemplatesInProgram, typeMap: " + typeMap)
      println("Current program:")
      import koolc.ast.Printer
      println(Printer.printTree(false)(program))

      def replaceType(tpe: TypeTree): TypeTree = tpe match {
          case id: Identifier => (typeMap.get(id) getOrElse tpe).setPos(tpe)
          case _              => tpe
        }

      def replaceInExpr(expr: ExprTree): ExprTree = {
        //println(s"replaceInExpr(${expr})")
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
          case MethodCall(obj, meth, args) => MethodCall(replaceInExpr(obj), meth, (args map replaceInExpr _)).setPos(expr)
          case NewIntArray(size)           => NewIntArray(replaceInExpr(size)).setPos(expr)
          case Not(expr)                   => Not(replaceInExpr(expr)).setPos(expr)
          case New(tpe)                    => New(replaceType(tpe).asInstanceOf[Identifier]).setPos(expr)
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

      def replaceTemplatesInMethod(method: MethodDecl): MethodDecl = {
        MethodDecl(
          retType = replaceType(method.retType),
          id = method.id,
          args = method.args map { arg => Formal(replaceType(arg.tpe), arg.id).setPos(arg) },
          vars = method.vars map { varDecl => VarDecl(replaceType(varDecl.tpe), varDecl.id).setPos(varDecl) },
          stats = method.stats map replaceTemplatesInStatement _,
          retExpr = replaceInExpr(method.retExpr),
          template = method.template).setPos(method)
      }

      def replaceTemplatesInClass(clazz: ClassDecl): ClassDecl = {
        ClassDecl(
          id = clazz.id,
          parent = clazz.parent flatMap { parent => typeMap.get(parent) orElse clazz.parent },
          vars = clazz.vars map { varDecl => VarDecl(replaceType(varDecl.tpe), varDecl.id) },
          methods = clazz.methods map replaceTemplatesInMethod _,
          template = clazz.template).setPos(clazz)
      }

      Program(
        MainObject(program.main.id, program.main.stats map replaceTemplatesInStatement _),
        program.classes map replaceTemplatesInClass _
      )
    }

    println("classTemplateReferences: " + classTemplateReferences)
    if(classTemplateReferences.isEmpty) {
      println("Done!")

      // Remove template classes
      val reducedProgram = Program(
        program.main,
        program.classes filter { _.template.isEmpty }
      )

      if(ctx.reporter.hasErrors) None
      else NameResolver.run(ctx)(reducedProgram, mainSymbol, classSymbols)
    } else {
      // Replace existing references

      val typeMap: Map[Identifier, Identifier] = (classTemplateReferences flatMap { (ref: Identifier) =>
        program.classes.find { clazz =>
          println(s"Class ID: ${clazz.id.value}, expanded id: ${expandClassId(ref, ref.template)}, equal: ${clazz.id.value == expandClassId(ref, ref.template).value}")
          clazz.id.value == expandClassId(ref, ref.template).value
        } map { clazz =>
          ref -> clazz.id
        }
      }).toMap
      println("typeMap: " + typeMap)
      val replacedProgram = replaceTemplatesInProgram(program, typeMap)

      // Expand nonexisting classes
      val newProgram = classTemplateReferences.foldLeft(replacedProgram)({ (program, ref) =>
        expandClassTemplate(program)(ref)
      })
      println("Recurse!")

      NameAnalysis.run(ctx)(Some(newProgram))
    }
  }

}
