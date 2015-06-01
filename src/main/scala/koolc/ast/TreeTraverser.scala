/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast

import Trees._

object TreeTraverser {

  private val any2true: PartialFunction[Any, Boolean]     = { case _ => true }
  private val any2Nil: PartialFunction[Any, Seq[Nothing]] = { case _ => Nil  }
  private val treeIdentity: PartialFunction[Tree, Tree]   = { case t => t    }

  def collect[T](
        t: Tree,
        branchFilter: PartialFunction[Tree, Boolean] = any2true,
        descendIntoDeclarationIds: Boolean = true,
        descendIntoTemplates: Boolean = true,
        methodCallIdsMatch: Boolean = true
      )(
        collector: PartialFunction[Tree, List[T]]
      ): List[T] = {

    def collectDeclarationId(id: Identifier): List[T] = if(descendIntoDeclarationIds) clct(id) else Nil
    def collectMethodCallId(id: Identifier): List[T] =
      if(methodCallIdsMatch) clct(id) else id.template flatMap clct

    def collectTemplateTree(tree: TemplateTree, value: => List[T]): List[T] =
      if(tree.template.isEmpty || descendIntoTemplates) value
      else Nil

    def clct(tree: Tree): List[T] = {
      val lst: List[T] = if((branchFilter orElse any2true)(tree)) tree match {
          case Program(main, classes) => clct(main) ++: (classes flatMap clct)
          case MainObject(id, stats)  => collectDeclarationId(id) ++: (stats flatMap clct)
          case clazz@ClassDecl(id, parent, vars, methods, template) =>
            collectTemplateTree(clazz,
              collectDeclarationId(id) ++:
              (parent map clct getOrElse Nil) ++:
              (vars flatMap clct) ++:
              (methods flatMap clct) ++:
              (template flatMap clct)
            )
          case VarDecl(tpe, id)       => clct(tpe) ++: collectDeclarationId(id)
          case method@MethodDecl(retType, id, args, vars, stats, retExpr, template) =>
            collectTemplateTree(method,
              clct(retType) ++:
              collectDeclarationId(method.id) ++:
              (args flatMap clct) ++:
              (vars flatMap clct) ++:
              (stats flatMap clct) ++:
              clct(retExpr) ++:
              (template flatMap clct)
            )
          case Formal(tpe, id)        => clct(tpe) ++: clct(id)

          case Block(stats)                 => stats flatMap clct
          case If(expr, thn, els)           => clct(expr) ++: clct(thn) ++: (els map clct getOrElse Nil)
          case While(expr, stat)            => clct(expr) ++: clct(stat)
          case Println(expr)                => clct(expr)
          case Assign(id, expr)             => clct(id) ++: clct(expr)
          case ArrayAssign(id, index, expr) => clct(id) ++: clct(index) ++: clct(expr)

          case And(lhs, rhs)               => clct(lhs) ++: clct(rhs)
          case Or(lhs, rhs)                => clct(lhs) ++: clct(rhs)
          case Plus(lhs, rhs)              => clct(lhs) ++: clct(rhs)
          case Minus(lhs, rhs)             => clct(lhs) ++: clct(rhs)
          case Times(lhs, rhs)             => clct(lhs) ++: clct(rhs)
          case Div(lhs, rhs)               => clct(lhs) ++: clct(rhs)
          case LessThan(lhs, rhs)          => clct(lhs) ++: clct(rhs)
          case Equals(lhs, rhs)            => clct(lhs) ++: clct(rhs)
          case Not(expr)                   => clct(expr)
          case ArrayRead(arr, index)       => clct(arr) ++: clct(index)
          case ArrayLength(arr)            => clct(arr)
          case MethodCall(obj, meth, args) => clct(obj) ++: collectMethodCallId(meth) ++: (args flatMap clct)
          case New(tpe)                    => clct(tpe)
          case NewIntArray(size)           => clct(size)

          case Identifier(_, template)     => template flatMap clct

          case True()         | False()      | This()           |
               IntArrayType() | IntType()    | BooleanType()    | StringType()   |
               IntLit(_)      | StringLit(_) => Nil
        } else Nil

      (collector orElse any2Nil)(tree) ++: lst
    }

    clct(t)
  }

  def transform[S <: Tree](
        t: S, branchFilter: PartialFunction[Tree, Boolean] = any2true,
        descendIntoTemplates: Boolean = true
      )(
        transform: PartialFunction[Tree, Tree]
      ): S = {

    def transformTemplateTree[T <: TemplateTree](tree: T, value: =>T): T =
      if(tree.template.isEmpty || descendIntoTemplates) value
      else tree

    def transformTree[T <: Tree](tree: T): T =
      (transform orElse treeIdentity)(
        if((branchFilter orElse any2true)(tree)) {
          (tree match {
            case Program(main, classes) => Program(transformTree(main), classes map transformTree).setPos(tree)
            case MainObject(id, stats)  => MainObject(transformTree(id), stats map transformTree).setPos(tree)
            case clazz@ClassDecl(id, parent, vars, methods, template) =>
              transformTemplateTree(clazz,
                ClassDecl(
                  transformTree(id),
                  parent map transformTree,
                  vars map transformTree,
                  methods map transformTree,
                  template
                )
              )
            case VarDecl(tpe, id)       => VarDecl(transformTree(tpe), transformTree(id))
            case method@MethodDecl(retType, id, args, vars, stats, retExpr, template) =>
              transformTemplateTree(method,
                MethodDecl(
                  transformTree(retType),
                  transformTree(id),
                  args map transformTree,
                  vars map transformTree,
                  stats map transformTree,
                  transformTree(retExpr),
                  template
                )
              )
            case Formal(tpe, id)        => Formal(transformTree(tpe), transformTree(id))

            case Block(stats)                 => Block(stats map transformTree)
            case If(expr, thn, els)           => If(transformTree(expr), transformTree(thn), els map transformTree)
            case While(expr, stat)            => While(transformTree(expr), transformTree(stat))
            case Println(expr)                => Println(transformTree(expr))
            case Assign(id, expr)             => Assign(transformTree(id), transformTree(expr))
            case ArrayAssign(id, index, expr) => ArrayAssign(transformTree(id), transformTree(index), transformTree(expr))

            case And(lhs, rhs)               => And(transformTree(lhs), transformTree(rhs))
            case Or(lhs, rhs)                => Or(transformTree(lhs), transformTree(rhs))
            case Plus(lhs, rhs)              => Plus(transformTree(lhs), transformTree(rhs))
            case Minus(lhs, rhs)             => Minus(transformTree(lhs), transformTree(rhs))
            case Times(lhs, rhs)             => Times(transformTree(lhs), transformTree(rhs))
            case Div(lhs, rhs)               => Div(transformTree(lhs), transformTree(rhs))
            case LessThan(lhs, rhs)          => LessThan(transformTree(lhs), transformTree(rhs))
            case Equals(lhs, rhs)            => Equals(transformTree(lhs), transformTree(rhs))
            case Not(expr)                   => Not(transformTree(expr))
            case ArrayRead(arr, index)       => ArrayRead(transformTree(arr), transformTree(index))
            case ArrayLength(arr)            => ArrayLength(transformTree(arr))
            case MethodCall(obj, meth, args) => MethodCall(transformTree(obj), transformTree(meth), args map transformTree)
            case New(tpe)                    => New(transformTree(tpe))
            case NewIntArray(size)           => NewIntArray(transformTree(size))

            case Identifier(value, template) => Identifier(value, template map transformTree)

            case True()         | False()      | This()         |
                 IntArrayType() | IntType()    | BooleanType()  |  StringType() |
                 IntLit(_)      | StringLit(_) => tree
          }).setPos(tree)
        } else tree
      ).asInstanceOf[T]

    transformTree(t)
  }
}
