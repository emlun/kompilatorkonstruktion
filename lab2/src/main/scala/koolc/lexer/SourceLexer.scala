package koolc
package lexer

import utils._
import scala.io.Source

object SourceLexer extends Pipeline[Source, Iterator[Token]] {
  import Tokens._

  val ALL_TOKEN_KINDS = Set(
    EOF,
    COLON,
    SEMICOLON,
    DOT,
    COMMA,
    EQSIGN,
    EQUALS,
    BANG,
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    AND,
    OR,
    LESSTHAN,
    PLUS,
    MINUS,
    TIMES,
    DIV,
    OBJECT,
    CLASS,
    DEF,
    VAR,
    UNIT,
    MAIN,
    STRING,
    EXTENDS,
    INT,
    BOOLEAN,
    WHILE,
    IF,
    ELSE,
    RETURN,
    LENGTH,
    TRUE,
    FALSE,
    THIS,
    NEW,
    PRINTLN,
    IDKIND,
    INTLITKIND,
    STRLITKIND)

  def run(ctx: Context)(source: Source): Iterator[Token] = {
    import ctx.reporter._

    // Complete this file

    var currentPos = source.pos
    var current: String = ""
    var candidates: Set[TokenKind] = ALL_TOKEN_KINDS

    var result: Seq[Token] = Nil

    for(next <- source) {
      if(current matches "^\\s*$") {
        current = "" + next
        currentPos = source.pos
      } else {
        candidates = candidates.filter(kind => Tokens.isPrefix(current, kind))

        val nextPrefix = current + next
        val nextCandidates = candidates.filter(kind => Tokens.isPrefix(nextPrefix, kind))

        if(nextCandidates isEmpty) {
          val kind =
            if(candidates.size == 1) candidates.head
            else (candidates - IDKIND).head

          val token = if(Tokens.isToken(current, kind)) {
               kind match {
                case INTLITKIND => new INTLIT(current.toInt)
                case STRLITKIND => new STRLIT(current)
                case IDKIND => new ID(current)
                case _ => new Token(kind)
              }
            } else {
              new Token(BAD)
            }
          token.setPos(ctx.file, currentPos)
          result = result :+ token

          current = "" + next
          currentPos = source.pos
          candidates = ALL_TOKEN_KINDS
        } else {
          current = nextPrefix
        }
      }
    }

    val nextCandidates = ALL_TOKEN_KINDS.filter(kind => Tokens.isPrefix(current, kind))

    if(!(current matches "^\\s*$")) {
      val kind =
        if(nextCandidates.size == 1) nextCandidates.head
        else (nextCandidates - IDKIND).head

      val token = if(Tokens.isToken(current, kind)) {
          kind match {
            case INTLITKIND => new INTLIT(current.toInt)
            case STRLITKIND => new STRLIT(current)
            case IDKIND => new ID(current)
            case _ => new Token(kind)
          }
        } else {
          new Token(BAD)
        }
      token.setPos(ctx.file, currentPos)
      result = result :+ token
    }

    val eof = new Token(EOF)
    eof.setPos(ctx.file, source.pos)
    (result :+ eof).toIterator
  }
}
