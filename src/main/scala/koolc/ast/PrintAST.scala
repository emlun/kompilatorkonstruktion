/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast

import Trees._

object PrintAST extends (Tree => String) {
  def apply(t: Tree): String = {
    t.toString
  }
}
