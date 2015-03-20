package koolc
package ast
package dsl

import scala.collection.mutable.ListBuffer

trait ParserDsl {

  sealed case class Accumulator[+T](val func: () => Option[T]) {
    def whileTrue(predicate: () => Boolean): List[T] = {
      var collection = new ListBuffer[T]
      while(predicate()) {
        collection ++= func()
      }
      collection.toList
    }
  }

  def accumulate[T](func: () => Option[T]) = Accumulator[T](func)
}
