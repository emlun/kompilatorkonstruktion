package koolc
package analyzer

import Symbols._

trait SymbolMatchers {
  import org.scalatest.matchers._

  class ShouldHaveSameSymbolMatcher(val right: Symbolic[_ <: Symbol]) extends Matcher[Symbolic[_ <: Symbol]] {
    override def apply(left: Symbolic[_ <: Symbol]) =
      left.symbol map { leftSymbol =>
        right.symbol map { rightSymbol =>
          MatchResult(leftSymbol.id == rightSymbol.id,
            s"Expected ${left} to have the same symbol (${leftSymbol.id}) as ${right} (${rightSymbol.id})",
            s"${left} has the same symbol (${leftSymbol.id}) as ${right} (${rightSymbol.id})"
          )
        } getOrElse MatchResult(false,
          s"Expected ${right} to have a symbol attached.",
          s"${right} has a symbol attached."
        )
      } getOrElse MatchResult(false,
        s"Expected ${left} to have a symbol attached.",
        s"${left} has a symbol attached."
      )
  }

  def haveSameSymbolAs(right: Symbolic[_ <: Symbol]) = new ShouldHaveSameSymbolMatcher(right)

}
