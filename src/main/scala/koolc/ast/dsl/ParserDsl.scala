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
    def whilst(test: => Boolean): List[T] =
      if(test) func() ++: whilst(test)
      else Nil
  }

  def accumulate[T](func: () => Option[T]) = new Accumulator[T](func)
}
