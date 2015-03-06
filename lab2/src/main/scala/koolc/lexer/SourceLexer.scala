package koolc
package lexer

import utils._
import scala.io.Source

object SourceLexer extends Pipeline[Source, Iterator[Token]] {
  import Tokens._

  private val ALL_TOKEN_KINDS = Set(EOF,
    COLON, SEMICOLON, DOT, COMMA, EQSIGN, EQUALS, BANG, LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE,
    AND, OR, LESSTHAN, PLUS, MINUS, TIMES, DIV,
    OBJECT, CLASS, DEF, VAR, UNIT, MAIN, STRING, EXTENDS, INT, BOOLEAN,
    WHILE, IF, ELSE, RETURN, LENGTH, TRUE, FALSE, THIS,
    NEW, PRINTLN, LINECOMMENT, BLOCKCOMMENT, IDKIND, INTLITKIND, STRLITKIND)

  private def makeToken(current: String, candidates: Set[TokenKind], ctx: Context, currentPos: Int) = {
    val matchedCandidates = candidates filter (kind => Tokens.isToken(current, kind))
    val kind = matchedCandidates.size match {
      case 1 => matchedCandidates.head
      case 0 => BAD
      case _ => (matchedCandidates - IDKIND).head
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

  private def readNextToken(ctx: Context, source: Source)(previous: String, prevPos: Int): Tuple3[Option[Token], String, Int] = {
    var current = previous
    var currentPos = prevPos

    var eatingLineComment = false
    var eatingBlockComment = false

    for(next <- source) {
      if(eatingLineComment) {
        if(current == "\n") {
          eatingLineComment = false
          current = ""
          currentPos = source.pos
        } else {
          current = next.toString
        }
      } else if(eatingBlockComment) {
        if(current + next == "*/") {
          eatingBlockComment = false
          current = ""
          currentPos = source.pos
        } else {
          current = next.toString
        }
      } else {
        if(current.trim.isEmpty) {
          current = ""
          currentPos = source.pos
        } else {
          current.trim match {
            case _ => {
              val candidateKinds = ALL_TOKEN_KINDS filter Tokens.isPrefix(current)
              if((candidateKinds filter Tokens.isPrefix(current + next)).isEmpty) {
                val token = makeToken(current, candidateKinds, ctx, currentPos)
                token.kind match {
                  case LINECOMMENT => {
                    eatingLineComment = true
                    current = ""
                  }
                  case BLOCKCOMMENT => {
                    eatingBlockComment = true
                    current = ""
                  }
                  case _ => { return (Some(token), next.toString, source.pos) }
                }
              }
            }
          }
        }
        current = current + next
      }
    }
    return (
      current.trim match {
        case "" => None
        case _  => Some(makeToken(current, ALL_TOKEN_KINDS filter Tokens.isPrefix(current), ctx, currentPos))
      },
      "",
      source.pos
    )
  }

  override def run(ctx: Context)(source: Source): Iterator[Token] = {
    val readNext = readNextToken(ctx, source) _
    var (nextToken, current, currentPos) = readNext("", source.pos)

    new Iterator[Token] {
      override def hasNext = nextToken match {
        case Some(_) => true
        case None    => false
      }

      override def next = {
        val result = nextToken match {
          case Some(t) => t
          case None    => null
        }

        val readResult = readNext(current, currentPos)
        current = readResult._2
        currentPos = readResult._3

        nextToken = readResult._1 match {
          case Some(t) => Some(t)
          case None    => nextToken match {
            case Some(t) => t.kind match {
              case EOF => None
              case _   => Some(new Token(EOF).setPos(ctx.file, source.pos))
            }
            case None    => None
          }
        }

        result
      }
    }
  }
}
