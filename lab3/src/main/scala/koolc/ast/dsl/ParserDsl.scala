package koolc
package ast
package dsl

trait ParserDsl {

  sealed case class Accumulator[+T](val func: () => Option[T]) {
    def whileTrue(predicate: () => Boolean): List[T] =
      if(predicate()) func() ++: whileTrue(predicate)
      else Nil
  }

  def accumulate[T](func: () => Option[T]) = Accumulator[T](func)
}
