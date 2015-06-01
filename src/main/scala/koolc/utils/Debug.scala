/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package utils

object Debug {

  private val DEBUG_ON = false

  def debug(): Unit =
    if(DEBUG_ON) {
      println()
    }

  def debug(value: => Any): Unit =
    if(DEBUG_ON) {
      println(value)
    }

}
