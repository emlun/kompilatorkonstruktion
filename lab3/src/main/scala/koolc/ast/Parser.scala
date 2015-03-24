package koolc
package ast

import scala.collection.mutable.ListBuffer

import dsl.ParserDsl
import utils._
import Trees._
import lexer._
import lexer.Tokens._


object Parser extends Pipeline[Iterator[Token], Option[Program]] with ParserDsl {

  val BEGIN_STATEMENT = List(LBRACE, IF, WHILE, PRINTLN, IDKIND)
  val BEGIN_EXPRESSION = List(INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS, NEW, BANG, LPAREN)

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

    def eatSequence(kinds: TokenKind*): Option[Token] = kinds.toList match {
      case head :: Nil  => eat(head)
      case head :: tail => eat(head) flatMap (_ => eatSequence(tail:_*))
      case Nil          => None
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
    def expected(kinds: TokenKind*): Unit = {
      ctx.reporter.error(
        s"expected: ${kinds.toList.mkString(" or ")}, found: ${currentToken}",
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

    def parseMainObject(): Option[MainObject] =
      eat(OBJECT) flatMap (_ =>
        eatIdentifier() flatMap (id =>
          eatSequence(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE) flatMap (_ => {
            val statements = accumulate(parseStatement) whilst(() => currentToken is (BEGIN_STATEMENT:_*))
            eatSequence(RBRACE, RBRACE) map (_ =>
              MainObject(id, statements)
            )
          })
        )
      )

    def parseClassDeclarations(): List[ClassDecl] = {
      def parseClassDeclaration(): Option[ClassDecl] = {
        def parseMethodDeclarations(): List[MethodDecl] = {
          def parseMethodDeclaration(): Option[MethodDecl] = {
            eat(DEF) flatMap (_ =>
              eatIdentifier() flatMap (id =>
                eat(LPAREN) flatMap (_ => {
                  val parameters = (if(currentToken is IDKIND) {
                    eatIdentifier() map (paramId => Formal(parseType(), paramId))
                  } else None) ++: (
                    accumulate(() =>
                      eat(COMMA) flatMap (_ =>
                        eatIdentifier() map (paramId => Formal(parseType(), paramId))
                      )
                    ) whilst(() => currentToken is COMMA))

                  eat(RPAREN) flatMap (_ => {
                    val returnType = parseType()
                    eatSequence(EQSIGN, LBRACE) flatMap (_ => {
                      val varDeclarations = parseVarDeclarations()
                      val statements = accumulate(parseStatement) whilst(() => currentToken is (BEGIN_STATEMENT:_*))

                      eat(RETURN) flatMap (_ => {
                        val returnExpression = parseExpression()
                        eatSequence(SEMICOLON, RBRACE) map (_ => {
                          MethodDecl(returnType, id,
                            parameters.toList, varDeclarations.toList, statements, returnExpression)
                        })
                      })
                    })
                  })
                })
              )
            )
          }

          accumulate(parseMethodDeclaration) whilst(() => currentToken is DEF)
        }

        eat(CLASS) flatMap (_ =>
          eatIdentifier() flatMap (id => {
            val parentClass = if(currentToken is EXTENDS) {
              eat(EXTENDS) flatMap (_ =>
                eatIdentifier()
              )
            } else None
            eat(LBRACE) flatMap (_ => {
              val vars = parseVarDeclarations()
              val methods = parseMethodDeclarations()
              eat(RBRACE) map (_ => ClassDecl(id, parentClass, vars, methods))
            })
          })
        )
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
        val statements = accumulate(parseStatement) whilst(() => currentToken is (BEGIN_STATEMENT:_*))
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
            readToken()
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
        case EOF     => null
        case _       => {
          expected(BEGIN_STATEMENT:_*)
          readToken()
          None
        }
      }
    }

    def parseMethodCall(obj: ExprTree): ExprTree = {
      val identifier = eatIdentifier().get;
      eat(LPAREN);
      var args = new ListBuffer[ExprTree];
      if(currentToken is (BEGIN_EXPRESSION:_*)) {
        args.append(parseExpression());
        while(currentToken is COMMA) {
          eat(COMMA)
          args.append(parseExpression());
        }
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
            maybeParseDot(ArrayLength(expression))
          } else {
            maybeParseDot(parseMethodCall(expression))
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
          case _           => {
            expected(BEGIN_EXPRESSION:_*)
            null
          }
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

    def maybeParseRightAnd(lhs: ExprTree): ExprTree = currentToken.kind match {
      case AND => { eat(AND); maybeParseRightAnd(And(lhs, parseComparison())) }
      case _   => lhs
    }

    def parseAnd(): ExprTree = maybeParseRightAnd(parseComparison())

    def maybeParseRightOr(lhs: ExprTree): ExprTree = currentToken.kind match {
      case OR  => { eat(OR);  maybeParseRightOr(Or(lhs, parseAnd())) }
      case _   => lhs
    }
    def parseExpression(): ExprTree = maybeParseRightOr(parseAnd())

    readToken()
    parseGoal()
  }
}
