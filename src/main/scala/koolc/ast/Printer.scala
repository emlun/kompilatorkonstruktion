/***
Authors:
Emil Lundberg
Petter Lundahl
***/

package koolc
package ast

import Trees._

object Printer extends (Tree => String) {
  def apply(t: Tree): String = t.print()
}
