package koolc
package ast

import scala.collection.mutable.ListBuffer

import dsl.ParserDsl
import utils._
import Trees._
import lexer._
import lexer.Tokens._


object Parser extends Pipeline[Iterator[Token], Option[Program]] with ParserDsl {
  def run(ctx: Context)(tokens: Iterator[Token]): Option[Program] = {
    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken(): Option[Token] = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken is BAD) {
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

    def eatSequence(kinds: TokenKind*): Seq[Token] = kinds.toList match {
      case head :: tail => eat(head) ++: eatSequence(tail:_*)
      case Nil          => Nil
    }

    def eatIdentifier(): Option[Identifier] = eat(IDKIND) match {
      case Some(ID(value)) => Some(new Identifier(value))
      case _ => {
        expected(IDKIND)
        None
      }
    }

    sealed case class DeferredTreeReturn[+T](val tree: T) {
      def thenEat(eatKinds: TokenKind*) = {
        eatSequence(eatKinds:_*)
        tree
      }
    }
    def firstReturn[T](returnTree: T) = DeferredTreeReturn[T](returnTree)

    /**
     * Complains that what was found was not expected. The method accepts
     * arbitrarily many arguments of type TokenKind.
     */
    def expected(kind: TokenKind, more: TokenKind*): Unit = {
      ctx.reporter.error(
        s"expected: ${(kind::more.toList).mkString(" or ")}, found: ${currentToken}",
        currentToken
      )
    }

    def parseGoal(): Option[Program] = parseMainObject() flatMap (main => {
      val program = Program(main, parseClassDeclarations())
      eat(EOF)

      if(ctx.reporter.hasErrors)
        None
      else
        Some(program)
    })

    def parseMainObject(): Option[MainObject] = {
      eat(OBJECT)

      eatIdentifier() map (id => {
        eatSequence(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE)

        val statements = accumulate(parseStatement) whilst(() => currentToken isnt RBRACE )

        eatSequence(RBRACE, RBRACE)

        MainObject(id, statements)
      })
    }

    def parseClassDeclarations(): List[ClassDecl] = {
      def parseClassDeclaration(): Option[ClassDecl] = {
        def parseMethodDeclarations(): List[MethodDecl] = {
          def parseMethodDeclaration(): Option[MethodDecl] = {
            eat(DEF)
            eatIdentifier() map (id => {
              eat(LPAREN)
              var parameters = new ListBuffer[Formal]
              while(currentToken isnt RPAREN) {
                if(parameters.length > 0) eat(COMMA)
                parameters ++= eatIdentifier() map ( paramId => Formal(parseType(), paramId) )
              }
              eat(RPAREN)
              val returnType = parseType()
              eatSequence(EQSIGN, LBRACE)

              val varDeclarations = parseVarDeclarations()

              val statements = accumulate(parseStatement) whilst(() => currentToken isnt RETURN)

              eat(RETURN)
              val returnExpression = parseExpression()
              eatSequence(SEMICOLON, RBRACE)
              MethodDecl(returnType, id,
                parameters.toList, varDeclarations.toList, statements, returnExpression)
            })
          }

          accumulate(parseMethodDeclaration) whilst(() => currentToken is DEF)
        }

        eat(CLASS)
        eatIdentifier() map (id => {
          val parentClass = if(currentToken is EXTENDS) { eat(EXTENDS); eatIdentifier() } else None
          eat(LBRACE);
          firstReturn(ClassDecl(id, parentClass, parseVarDeclarations(), parseMethodDeclarations()))
            .thenEat(RBRACE)
        }) orElse None
      }

      accumulate(parseClassDeclaration) whilst(() => currentToken is CLASS)
    }

    def parseVarDeclarations(): List[VarDecl] = {
      def parseVarDeclaration(): Option[VarDecl] = {
        eat(VAR)
        eatIdentifier() map (id => {
          firstReturn(VarDecl(parseType(), id)) thenEat(SEMICOLON)
        })
      }

      accumulate(parseVarDeclaration) whilst(() => currentToken is VAR)
    }

    def parseType(): TypeTree = {
      eat(COLON)
      currentToken.kind match {
        case BOOLEAN => { eat(BOOLEAN); new BooleanType }
        case STRING  => { eat(STRING);  new StringType }
        case IDKIND  => eatIdentifier().get
        case INT     => {
          eat(INT)
          if(currentToken is LBRACKET) {
            eatSequence(LBRACKET, RBRACKET)
            new IntArrayType
          } else new IntType
        }
        case _ => {
          expected(BOOLEAN, STRING, IDKIND, INT)
          null
        }
      }
    }

    def parseStatement(): Option[StatTree] = {

      def parseBlock(): Option[StatTree] = {
        eat(LBRACE)
        val statements = accumulate(parseStatement) whilst(() => currentToken isnt RBRACE)
        eat(RBRACE)
        Some(Block(statements))
      }

      def parseIf(): Option[If] = {
        eatSequence(IF, LPAREN)
        val expression = parseExpression()
        eat(RPAREN)
        parseStatement() map (thenStatement => {
          val elseStatement = if(currentToken is ELSE) {
            eat(ELSE)
            parseStatement()
          } else None

          If(expression, thenStatement, elseStatement)
        })
      }

      def parseWhile(): Option[While] = {
        eatSequence(WHILE, LPAREN)
        val expression = parseExpression()
        eat(RPAREN)
        parseStatement() map ( doStatement => While(expression, doStatement) )
      }

      def parsePrintln(): Option[Println] = {
        eatSequence(PRINTLN, LPAREN)
        Some(Println(firstReturn(parseExpression()) thenEat(RPAREN, SEMICOLON)))
      }

      def parseAssignment(): Option[StatTree] = eatIdentifier() flatMap (assignId =>
        currentToken.kind match {
          case EQSIGN   => {
            eat(EQSIGN)
            Some(Assign(assignId, firstReturn(parseExpression()) thenEat(SEMICOLON)))
          }
          case LBRACKET => {
            eat(LBRACKET)
            val index = firstReturn(parseExpression()) thenEat(RBRACKET, EQSIGN)
            val value = firstReturn(parseExpression()) thenEat(SEMICOLON)
            Some(ArrayAssign(assignId, index, value))
          }
          case _        => {
            expected(EQSIGN, LBRACKET)
            None
          }
        }
      )

      currentToken.kind match {
        case LBRACE  => parseBlock()
        case IF      => parseIf()
        case WHILE   => parseWhile()
        case PRINTLN => parsePrintln()
        case IDKIND  => parseAssignment()
        case _       => {
          expected(LPAREN, IF, WHILE, PRINTLN, IDKIND)
          readToken()
          None
        }
      }
    }

    def parseMethodCall(obj: ExprTree): ExprTree = {
      val identifier = eatIdentifier().get;
      eat(LPAREN);
      var args = new ListBuffer[ExprTree];
      while(currentToken.kind != RPAREN) {
        args.append(parseExpression());
        if(currentToken is COMMA)
          eat(COMMA)
      }
      eat(RPAREN);
      MethodCall(obj, identifier, args.toList)
    }

    def parseNegation(): ExprTree = {

      def parseNew(): ExprTree = {
        eat(NEW);
        if(currentToken is IDKIND) {
          firstReturn(New(eatIdentifier().get)) thenEat(LPAREN, RPAREN)
        } else {
          eatSequence(INT, LBRACKET)
          firstReturn(NewIntArray(parseExpression())) thenEat(RBRACKET)
        }
      }

      def maybeParseDot(expression: ExprTree): ExprTree = {
        if(currentToken is DOT) {
          eat(DOT);
          if(currentToken is LENGTH) {
            eat(LENGTH);
            return ArrayLength(expression);
          } else {
            return parseMethodCall(expression);
          }
        } else expression
      }

      def maybeParseArrayRead(expression: ExprTree): ExprTree = {
        if(currentToken is LBRACKET) {
          eat(LBRACKET);
          firstReturn(ArrayRead(expression, parseExpression())) thenEat(RBRACKET)
        } else expression
      }

      def parseExpressionBase() = currentToken match {
        case INTLIT(value) => { eat(INTLITKIND); IntLit(value) }
        case STRLIT(value) => { eat(STRLITKIND); StringLit(value) }
        case ID(value)     => { eat(IDKIND);     Identifier(value) }
        case _             => currentToken.kind match {
          case TRUE        => { eat(TRUE); new True }
          case FALSE       => { eat(FALSE); new False }
          case THIS        => { eat(THIS); new This }
          case NEW         => parseNew()
          case BANG        => { eat(BANG); Not(parseNegation()) }
          case LPAREN      => { eat(LPAREN); firstReturn(parseExpression()) thenEat(RPAREN) }
          case _           => ???
        }
      }

      maybeParseArrayRead(maybeParseDot(parseExpressionBase()))
    }

    def maybeParseRightFactor(lhs: ExprTree): ExprTree = currentToken.kind match {
      case TIMES => { eat(TIMES); maybeParseRightFactor(Times(lhs, parseNegation())) }
      case DIV   => { eat(DIV);   maybeParseRightFactor(Div(lhs, parseNegation())) }
      case _     => lhs
    }
    def parseProduct(): ExprTree = maybeParseRightFactor(parseNegation())

    def maybeParseRightTerm(lhs: ExprTree): ExprTree = currentToken.kind match {
      case PLUS  => { eat(PLUS);  maybeParseRightTerm(Plus(lhs, parseProduct())) }
      case MINUS => { eat(MINUS); maybeParseRightTerm(Minus(lhs, parseProduct())) }
      case _     => lhs
    }
    def parseSum(): ExprTree = maybeParseRightTerm(parseProduct())

    def maybeParseRightComparee(lhs: ExprTree): ExprTree = currentToken.kind match {
      case LESSTHAN => { eat(LESSTHAN); maybeParseRightComparee(LessThan(lhs, parseSum())) }
      case EQUALS   => { eat(EQUALS);   maybeParseRightComparee(Equals(lhs, parseSum())) }
      case _        => lhs
    }
    def parseComparison(): ExprTree = maybeParseRightComparee(parseSum())

    def maybeParseRightExpression(lhs: ExprTree): ExprTree = currentToken.kind match {
      case AND => { eat(AND); maybeParseRightExpression(And(lhs, parseComparison())) }
      case OR  => { eat(OR);  maybeParseRightExpression(Or(lhs, parseComparison())) }
      case _   => lhs
    }
    def parseExpression(): ExprTree = maybeParseRightExpression(parseComparison())

    readToken()
    parseGoal()
  }
}
