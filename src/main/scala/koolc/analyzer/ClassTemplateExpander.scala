/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package analyzer

import utils._
import ast.Trees._

object ClassTemplateExpander extends Pipeline[Option[Program], Option[Program]] {

  override def run(ctx: Context)
    (program: Option[Program]): Option[Program] = program flatMap { program =>

    def expandClassId(classId: Identifier, template: List[TypeTree]): Identifier =
      Identifier(classId.value + "$" + (template map { _.name2 } mkString ","), Nil).setPos(classId)

    def getClassTemplateReferences(program: Program): List[Identifier] = {
      def getInType(tpe: TypeTree): List[Identifier] = tpe match {
          case Identifier(value, Nil)         => Nil
          case id@Identifier(value, template) => (template flatMap getInType _) match {
              case Nil      => List(id)
              case nonempty => nonempty
            }
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

    def expandClassTemplate(program: Program)(reference: Identifier): Program = {
      def expandInProgram(program: Program, reference: Identifier): Option[Program] = {
        def internal(program: Program, reference: Identifier): Option[Program] = {
          val types = reference.template
          program.classes find { _.id.value == reference.value } orElse {
            ctx.reporter.error(s"Template class ${reference.value} not found.", reference)
            None
          } flatMap { clazz =>
            if(clazz.template.size != types.size) {
              ctx.reporter.error(
                s"Wrong number of type parameters for class ${clazz.id.value} (expected ${clazz.template.size}, got ${types.size}: ${types map { _.name } mkString ","})", reference)
              None
            } else {
              Some(clazz)
            }
          } flatMap { clazz =>
            val typeMap: Map[String, TypeTree] = (clazz.template map { _.value } zip types).toMap

            def expandTypeTree(tpe: TypeTree): TypeTree = {
              tpe match {
                case id@Identifier(value, template) => typeMap.get(value) match {
                  case Some(Identifier(templateValue, templateValueTemplate)) =>
                    Identifier(templateValue, templateValueTemplate map expandTypeTree _).setPos(id)
                  case Some(templateValue) => templateValue
                  case None                => Identifier(value, template map expandTypeTree _).setPos(id)
                }
                case _                     => tpe
              }
            }

            def expandInExpr(expr: ExprTree): ExprTree = {
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
                case New(tpe)                    => expandTypeTree(tpe) match {
                  case id: Identifier => New(id).setPos(expr)
                  case other => {
                    ctx.reporter.error("Expected class type, found " + other, tpe)
                    IntLit(0).setPos(tpe)
                  }
                }
                case This()                      => This()
                case id: Identifier              => id.copy().setPos(id)
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
                case Assign(id, expr)             => Assign(id.copy(), expandInExpr(expr)).setPos(statement)
                case ArrayAssign(id, index, expr) => ArrayAssign(
                    id.copy(),
                    expandInExpr(index),
                    expandInExpr(expr)
                  ).setPos(statement)
              }

            def expandTemplateReferencesInMethod(method: MethodDecl): MethodDecl = {
              MethodDecl(
                retType = expandTypeTree(method.retType),
                id = method.id.copy(),
                args = method.args map { arg => Formal(expandTypeTree(arg.tpe), arg.id.copy()).setPos(arg) },
                vars = method.vars map { varDecl => VarDecl(expandTypeTree(varDecl.tpe), varDecl.id.copy()).setPos(varDecl) },
                stats = method.stats map expandTemplateReferencesInStatement _,
                retExpr = expandInExpr(method.retExpr),
                template = method.template).setPos(method)
            }

            val newClassId = expandClassId(clazz.id, types)
            program.classes find { clazz => clazz.id.value == newClassId.value } match {
              case Some(_) => None
              case None => {
                val newDecl = ClassDecl(
                  id = newClassId.copy(),
                  parent = clazz.parent map { parent => expandTypeTree(parent).asInstanceOf[Identifier] },
                  vars = clazz.vars map { varDecl => VarDecl(expandTypeTree(varDecl.tpe), varDecl.id.copy()).setPos(varDecl) },
                  methods = clazz.methods map expandTemplateReferencesInMethod _,
                  template = Nil).setPos(clazz)
                Some(newDecl)
              }
            }
          }
        } map { clazz =>
          Program(program.main, clazz +: program.classes)
        } orElse {
          None
        }

        val result = reference.template.foldLeft[Either[Program,Program]](Left(program)){ (prg, ref) => {
            val program = prg.fold((p => p), (p => p)) // Unpack the value from the Either
            ref match {
              case id@Identifier(_, head :: tail) => {
                expandInProgram(program, id) orElse {
                  internal(program, id) // Only expand this level if child levels had nothing to expand
                } map {
                  Right(_) // If either this or a child level expanded something, signal parents to not expand
                } getOrElse prg
              }
              case _ => {
                prg
              }
            }
          }
        }
        result match {
          case Left(program)  => internal(program, reference) // No descendant had anything to expand
          case Right(program) => Some(program)                // Some descendant had something to expand
        }
      }

      expandInProgram(program, reference) getOrElse program
    }

    def replaceTemplatesInProgram(program: Program, typeMap: Map[Identifier, Identifier]): Program = {
      def replaceType(tpe: TypeTree): TypeTree = tpe match {
          case id: Identifier => (typeMap.get(id) map { newId =>
              Identifier(newId.value, newId.template).setPos(id)
            } orElse { Some(id) } map { id =>
              Identifier(id.value, id.template map replaceType _).setPos(id)
            } getOrElse tpe).setPos(tpe)
          case _              => tpe
        }

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
          id = method.id.copy(),
          args = method.args map { arg => Formal(replaceType(arg.tpe), arg.id.copy()).setPos(arg) },
          vars = method.vars map { varDecl => VarDecl(replaceType(varDecl.tpe), varDecl.id.copy()).setPos(varDecl) },
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

    program.classes filter { ! _.template.isEmpty } foreach { clazz =>
      clazz.template.foldLeft[Set[Identifier]](
        program.classes.map(clazz => Identifier(clazz.id.value, Nil).setPos(clazz.id)).toSet
      ) { (allIds, paramId) =>
        allIds find { _ == paramId } map { existingId =>
          ctx.reporter.error(s"Template parameter name collision: '${paramId.value}'", paramId)
          ctx.reporter.info(s"Name '${paramId.value}' first defined here:", existingId)
          allIds
        } getOrElse {
          allIds + paramId
        }
      }
    }

    if(ctx.reporter.hasErrors) {
      return None
    }

    val classTemplateReferences = getClassTemplateReferences(program).toSet

    if(classTemplateReferences.isEmpty) {
      if(ctx.reporter.hasErrors) None
      else NameAnalysis.run(ctx)(Some(program))
    } else {
      // Replace existing references

      val typeMap: Map[Identifier, Identifier] = (classTemplateReferences flatMap { (ref: Identifier) =>
        program.classes.find { clazz =>
          clazz.id.value == expandClassId(ref, ref.template).value
        } map { clazz =>
          ref -> clazz.id
        }
      }).toMap
      val replacedProgram = replaceTemplatesInProgram(program, typeMap)

      // Expand nonexisting classes
      val newProgram = classTemplateReferences.foldLeft(replacedProgram)({ (program, ref) =>
        expandClassTemplate(program)(ref)
      })

      ctx.recursionCount += 1
      if(ctx.recursionCount > 100){
        ctx.reporter.error("Infinite template recursion detected at depth 100.",classTemplateReferences.head)
        None
      }
      else if(ctx.reporter.hasErrors) None
      else run(ctx)(Some(newProgram))
    }
  }

}
