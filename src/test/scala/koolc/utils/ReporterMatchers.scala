package koolc
package utils

trait ReporterMatchers {
  import org.scalatest.matchers._

  class ReporterShouldBeErrorlessMatcher extends BeMatcher[Reporter] {
    override def apply(left: Reporter) = {
      val message = left.messages map (_.toString) mkString "\n"
      MatchResult(!left.hasErrors, message, "Reporter has no messages.")
    }
  }

  def errorless = new ReporterShouldBeErrorlessMatcher
}
