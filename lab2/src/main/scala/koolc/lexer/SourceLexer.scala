package koolc
package lexer

import utils._
import scala.io.Source

object SourceLexer extends Pipeline[Source, Iterator[Token]] {
  import Tokens._

  val ALL_TOKEN_KINDS = Set(EOF,
    COLON, SEMICOLON, DOT, COMMA, EQSIGN, EQUALS, BANG, LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE,
    AND, OR, LESSTHAN, PLUS, MINUS, TIMES, DIV,
    OBJECT, CLASS, DEF, VAR, UNIT, MAIN, STRING, EXTENDS, INT, BOOLEAN,
    WHILE, IF, ELSE, RETURN, LENGTH, TRUE, FALSE, THIS,
    NEW, PRINTLN, IDKIND, INTLITKIND, STRLITKIND)

  def makeToken(current: String, candidates: Set[TokenKind], ctx: Context, currentPos: Int) = {
    val kind = candidates.size match {
      case 1 => candidates.head
      case 0 => BAD
      case _ => (candidates - IDKIND).head
    }

    val token = if(Tokens.isToken(current, kind)) {
        kind match {
          case INTLITKIND => new INTLIT(current.toInt)
          case STRLITKIND => new STRLIT(current)
          case IDKIND     => new ID(current)
          case _          => new Token(kind)
        }
      } else {
        new Token(BAD)
      }
    token.setPos(ctx.file, currentPos)
  }

  def readNextToken(ctx: Context, source: Source)(previous: String, prevPos: Int): Tuple3[Option[Token], String, Int] = {
    var current = previous
    var currentPos = prevPos
    for(next <- source) {
      if(current.trim.isEmpty) {
        current = ""
        currentPos = source.pos
      } else {
        val candidateKinds = ALL_TOKEN_KINDS filter Tokens.isPrefix(current)
        if((candidateKinds filter Tokens.isPrefix(current + next)).isEmpty) {
          return (Some(makeToken(current, candidateKinds, ctx, currentPos)), next.toString, source.pos)
        }
      }
      current = current + next
    }
    return (None, current, currentPos)
  }

  def run(ctx: Context)(source: Source): Iterator[Token] = {
    var currentPos = source.pos
    var current: String = ""

    var result: Seq[Token] = Nil
    val readNext = readNextToken(ctx, source) _

    while(source.hasNext) {
      val readResult = readNext(current, currentPos)
      val token = readResult._1
      current = readResult._2
      currentPos = readResult._3

      token match {
        case Some(t) => result = result :+ t
        case None => {}
      }
    }

    if(!current.trim.isEmpty) {
      result = result :+ makeToken(current, ALL_TOKEN_KINDS filter { Tokens.isToken(current, _) }, ctx, currentPos)
    }

    (result :+ new Token(EOF).setPos(ctx.file, source.pos)).toIterator
  }
}
