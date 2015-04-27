package koolc
package analyzer

import Symbols._

trait SymbolMatchers {
  import org.scalatest.matchers._

  class ShouldHaveSameSymbolMatcher(val right: Symbolic[_ <: Symbol]) extends Matcher[Symbolic[_ <: Symbol]] {
    override def apply(left: Symbolic[_ <: Symbol]) =
      MatchResult(left.symbol.id == right.symbol.id,
        s"Expected ${left} to have the same symbol (${left.symbol.id}) as ${right} (${right.symbol.id})",
        s"${left} has the same symbol (${left.symbol.id}) as ${right} (${right.symbol.id})"
      )
  }

  def haveSameSymbolAs(right: Symbolic[_ <: Symbol]) = new ShouldHaveSameSymbolMatcher(right)

}
