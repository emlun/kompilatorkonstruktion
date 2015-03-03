package koolc
package lexer

import scala.io.Source

import org.scalatest.FunSpec
import org.scalatest.Matchers

import utils._


object StringToSource extends Pipeline[String, Source] {
  override def run(ctx : Context)(v : String): Source = Source.fromString(v)
}

class SourceLexerSpec extends FunSpec with Matchers {

  implicit class TokenSeqExpector(val tokenSeq : Seq[TokenKind])
    extends Pipeline[Iterator[Token], Boolean] {

    override def run(ctx : Context)(v : Iterator[Token]) : Boolean = {
      tokenSeq.corresponds(v.toList)((k: TokenKind, t: Token) => {
        println(s"Processing token $t")
        t.kind == k
      })
    }
  }

  describe("The SourceLexer") {
    import Tokens._

    val ctx = Context(new Reporter, None, null)
    val lexer = StringToSource andThen SourceLexer

    it("does something") {
      val source = """foo"""
      val pipeline = lexer andThen (PRINTLN :: LPAREN :: STRLITKIND :: RPAREN :: EOF :: Nil)
      pipeline.run(ctx)(source) should be (true)
    }
  }

}
