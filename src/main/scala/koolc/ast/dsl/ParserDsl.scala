/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast
package dsl

trait ParserDsl {

  sealed class Accumulator[+T](val generator: () => Option[T]) {
    def whilst(test: => Boolean): List[T] =
      if(test) generator() ++: whilst(test)
      else Nil
  }

  def accumulate[T](generator: => Option[T]) = new Accumulator[T](() => generator)
}
