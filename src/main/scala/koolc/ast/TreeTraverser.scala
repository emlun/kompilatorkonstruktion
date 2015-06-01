/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast

import Trees._

object TreeTraverser {

  private val tree2true: PartialFunction[Tree, Boolean] = { case _ => true }
  private val treeIdentity: PartialFunction[Tree, Tree] = { case t => t }

  def transform[S <: Tree]
      (t: S, branchFilter: PartialFunction[Tree, Boolean] = tree2true)
      (transform: PartialFunction[Tree, Tree])
      : S = {

    def transformTree[T <: Tree](tree: T): T =
      (transform orElse treeIdentity)(
        if((branchFilter orElse tree2true)(tree)) {
          (tree match {
            case Program(main, classes) => Program(transformTree(main), classes map transformTree).setPos(tree)
            case MainObject(id, stats)  => MainObject(transformTree(id), stats map transformTree).setPos(tree)
            case ClassDecl(id, parent, vars, methods, template) =>
              ClassDecl(
                transformTree(id),
                parent map transformTree,
                vars map transformTree,
                methods map transformTree,
                template
              )
            case VarDecl(tpe, id)       => VarDecl(transformTree(tpe), transformTree(id))
            case MethodDecl(retType, id, args, vars, stats, retExpr, template) =>
              MethodDecl(
                transformTree(retType),
                transformTree(id),
                args map transformTree,
                vars map transformTree,
                stats map transformTree,
                transformTree(retExpr),
                template
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

            case True()         | False()      | This()           |
                 IntArrayType() | IntType()    | BooleanType()    | StringType()   |
                 IntLit(_)      | StringLit(_) | Identifier(_, _) | NewIntArray(_) => tree
          }).setPos(tree)
        } else tree
      ).asInstanceOf[T]

    transformTree(t)
  }
}
