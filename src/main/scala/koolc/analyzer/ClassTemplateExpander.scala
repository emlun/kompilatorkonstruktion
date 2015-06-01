/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package analyzer

import utils._
import ast.Trees._
import ast.TreeTraverser

object ClassTemplateExpander extends Pipeline[Option[Program], Option[Program]] {

  override def run(ctx: Context)
    (program: Option[Program]): Option[Program] = program flatMap { program =>
    println("ClassTemplateExpander.run")

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

      (TreeTraverser.collect(program,
        descendIntoDeclarationIds = false,
        methodCallIdsMatch = false,
        descendIntoTemplates = false) {
        case tpe: TypeTree => getInType(tpe)
      }).toSet.toList
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
            val newClassId = expandClassId(clazz.id, types)

            if(program.classes exists { clazz => clazz.id.value == newClassId.value }) {
              None
            } else {
              Some(
                TreeTraverser.transform(clazz) {
                  case id@Identifier(value, template) => typeMap.get(value) map {
                      case Identifier(typeMapValue, _) => Identifier(typeMapValue, template).setPos(id)
                      case other                       => other
                    } getOrElse id
                  case ths: This     => ths.copy().setPos(ths)
                  case expr@New(tpe) => tpe match {
                      case id: Identifier => New(id).setPos(expr)
                      case other          => {
                        ctx.reporter.error("Expected class type, found " + other, tpe)
                        IntLit(0).setPos(tpe)
                      }
                    }
                  case clazz@ClassDecl(id, parent, vars, methods, template) =>
                    clazz.copy(
                      id = newClassId.copy().setPos(id),
                      template = Nil
                    ).setPos(clazz)
                }
              )
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

      TreeTraverser.transform(program, descendIntoTemplates = false) {
        case tpe: TypeTree => replaceType(tpe)
      }
    }

    ctx.recursionCount += 1
    if(ctx.recursionCount > 100){
      ctx.reporter.error("Infinite template recursion detected at depth 100.")
      return None
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

      if(ctx.reporter.hasErrors) None
      else run(ctx)(Some(newProgram))
    }
  }

}
