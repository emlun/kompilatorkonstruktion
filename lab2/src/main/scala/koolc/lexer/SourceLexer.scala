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

  def makeToken(current: String, candidates: Set[TokenKind], ctx: Context, currentPos: Int) = {
    val kind =
      if(candidates.size == 1) candidates.head
      else if(candidates.size == 0) BAD
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
    token
  }

  def run(ctx: Context)(source: Source): Iterator[Token] = {
    import ctx.reporter._

    // Complete this file

    var currentPos = source.pos
    var current: String = ""

    var result: Seq[Token] = Nil

    while(source.hasNext) {
      val next = source.next

      if(current.trim.isEmpty) {
        current = next.toString
        currentPos = source.pos
      } else {
        val candidates = ALL_TOKEN_KINDS.filter(kind => Tokens.isPrefix(current, kind))

        val nextPrefix = current + next
        val nextCandidates = candidates.filter(kind => Tokens.isPrefix(nextPrefix, kind))

        if(nextCandidates.isEmpty) {
          result = result :+ makeToken(current, candidates, ctx, currentPos)

          current = next.toString
          currentPos = source.pos
        } else {
          current = nextPrefix
        }
      }
    }

    if(!current.trim.isEmpty) {
      result = result :+ makeToken(current, ALL_TOKEN_KINDS filter { Tokens.isToken(current, _) }, ctx, currentPos)
    }

    val eof = new Token(EOF)
    eof.setPos(ctx.file, source.pos)
    (result :+ eof).toIterator
  }
}
