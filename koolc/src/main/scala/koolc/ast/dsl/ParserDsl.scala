/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast
package dsl

trait ParserDsl {

  sealed class Accumulator[+T](val func: () => Option[T]) {
    def whilst(predicate: () => Boolean): List[T] =
      if(predicate()) func() ++: whilst(predicate)
      else Nil
  }

  def accumulate[T](func: () => Option[T]) = new Accumulator[T](func)
}
