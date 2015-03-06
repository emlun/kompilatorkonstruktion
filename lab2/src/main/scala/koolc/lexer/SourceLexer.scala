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
    return (
      Some(makeToken(current, ALL_TOKEN_KINDS filter Tokens.isPrefix(current), ctx, currentPos)),
      "",
      source.pos
    )
  }

  def run(ctx: Context)(source: Source): Iterator[Token] = {
    val readNext = readNextToken(ctx, source) _
    var (nextToken, current, currentPos) = readNext("", source.pos)

    new Iterator[Token] {
      override def hasNext = nextToken match {
        case Some(_) => true
        case None    => false
      }

      override def next = {
        val result = nextToken
        nextToken =
          if(source.hasNext) {
            val readResult = readNext(current, currentPos)
            current = readResult._2
            currentPos = readResult._3

            readResult._1
          } else {
            if(!current.trim.isEmpty) {
              val nextNext = makeToken(
                current,
                ALL_TOKEN_KINDS filter { Tokens.isToken(current, _) },
                ctx,
                currentPos
              )
              current = ""
              Some(nextNext)
            } else {
              nextToken match {
                case Some(t) =>
                  if(t.kind == EOF) None
                  else              Some(new Token(EOF).setPos(ctx.file, source.pos))
                case None => None
              }
            }
          }

        result match {
          case Some(t) => t
          case None => null
        }
      }
    }
  }
}
