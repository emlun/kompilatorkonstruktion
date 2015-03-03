package koolc
package lexer

import utils._
import scala.io.Source

object SourceLexer extends Pipeline[Source, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(source: Source): Iterator[Token] = {
    import ctx.reporter._

    // Complete this file

    new Iterator[Token] {
      def hasNext = {
        ???
      }

      def next = {
        ???
      }
    }

  }
}
