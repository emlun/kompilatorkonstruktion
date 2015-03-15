package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken(): Option[Token] = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }

        Some(currentToken)
      } else {
        None
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Option[Token] = {
      if (currentToken.kind == kind) {
        val current = Some(currentToken)
        readToken
        current
      } else {
        expected(kind)
        None
      }
    }

    def eatIdentifier(): Option[Identifier] = eat(IDKIND) match {
      case Some(ID(value)) => Some(new Identifier(value))
      case _ => {
        expected(IDKIND)
        None
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Unit = {
      ctx.reporter.error("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal(): Program = {
      val program = new Program(parseMainObject(), Nil)
      eat(EOF)
      program
    }

    def parseMainObject(): MainObject = {
      eat(OBJECT)

      (
        eatIdentifier() map (id => {
          eat(LBRACE)
          eat(DEF)
          eat(MAIN)
          eat(LPAREN)
          eat(RPAREN)
          eat(COLON)
          eat(UNIT)
          eat(EQSIGN)
          eat(LBRACE)

          var statementsStack: List[StatTree] = Nil
          while(currentToken.kind != RBRACE) {
            statementsStack = parseStatement() :: statementsStack
          }

          eat(RBRACE)
          eat(RBRACE)

          MainObject(id, statementsStack.reverse)
        }) orElse Some(MainObject(null, Nil))
      ).get
    }

    def parseClassDeclaration(): ClassDecl = ???

    def parseStatement(): StatTree = {

      def parseBlock(): StatTree = ???
      def parseIf(): If = ???
      def parseWhile(): While = ???

      def parsePrintln(): Println = {
        eat(PRINTLN)
        eat(LPAREN)
        val expression = parseExpression()
        eat(RPAREN)
        eat(SEMICOLON)
        new Println(expression)
      }

      def parseAssignment(): StatTree = ???

      currentToken.kind match {
        case LPAREN  => parseBlock()
        case IF      => parseIf()
        case WHILE   => parseWhile()
        case PRINTLN => parsePrintln()
        case IDKIND  => parseAssignment()
        case _       => {
          expected(LPAREN, IF, WHILE, PRINTLN, IDKIND)
          readToken()
          new Block(Nil)
        }
      }
    }

    def parseExpression(): ExprTree = ???

    readToken()
    val tree = parseGoal()
    tree
  }
}
