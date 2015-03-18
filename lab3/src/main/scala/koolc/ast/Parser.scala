package koolc
package ast

import scala.collection.mutable.ListBuffer

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Option[Program]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Option[Program] = {
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

    def parseGoal(): Option[Program] = {
      val program = new Program(parseMainObject(), parseClassDeclarations())
      eat(EOF)

      if(ctx.reporter.hasErrors)
        None
      else
        Some(program)
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

          var statements = new ListBuffer[StatTree]
          while(currentToken.kind != RBRACE) {
            statements += parseStatement()
          }

          eat(RBRACE)
          eat(RBRACE)

          MainObject(id, statements.toList)
        }) orElse Some(MainObject(null, Nil))
      ).get
    }

    def parseClassDeclarations(): List[ClassDecl] = {
      def parseClassDeclaration(): Option[ClassDecl] = {
        def parseMethodDeclarations(): List[MethodDecl] = ???

        eat(CLASS)
        eatIdentifier() map (id => {
          val parentClass = if(currentToken.kind == EXTENDS) { eat(EXTENDS); eatIdentifier() } else None
          eat(LBRACE);
          val classDeclaration = ClassDecl(id, parentClass, parseVarDeclarations(), parseMethodDeclarations())
          eat(RBRACE);
          classDeclaration
        }) orElse None
      }

      val classes = new ListBuffer[ClassDecl]
      while(currentToken.kind == CLASS) {
        classes ++= parseClassDeclaration()
      }
      classes.toList
    }

    def parseVarDeclarations(): List[VarDecl] = ???

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

      def parseAssignment(): StatTree = {
        (eatIdentifier() flatMap (assignId =>
          currentToken.kind match {
            case EQSIGN   => {
              eat(EQSIGN)
              val value = parseExpression()
              eat(SEMICOLON)
              Some(Assign(assignId, value))
            }
            case LBRACKET => {
              eat(LBRACKET)
              val index = parseExpression()
              eat(RBRACKET)
              eat(EQSIGN)
              val value = parseExpression()
              eat(SEMICOLON);
              Some(ArrayAssign(assignId, index, value))
            }
            case _        => {
              expected(EQSIGN, LBRACKET)
              None
            }
          }
        )).getOrElse(new Block(Nil))
      }

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

    def parseExpression(): ExprTree = {

      def parseNew(): ExprTree = ???

      currentToken match {
        case INTLIT(value) => { eat(INTLITKIND); new IntLit(value) }
        case STRLIT(value) => { eat(STRLITKIND); new StringLit(value) }
        case ID(value)     => { eat(IDKIND);     new Identifier(value) }
        case _             => currentToken.kind match {
          case TRUE        => { eat(TRUE); new True }
          case FALSE       => { eat(FALSE); new False }
          case THIS        => { eat(THIS); new This }
          case NEW         => parseNew()
          case BANG        => ???
          case LPAREN      => { eat(LPAREN); val expression = parseExpression(); eat(RPAREN); expression }
          case _           => ???
        }
      }
    }

    readToken()
    parseGoal()
  }
}
