/***
Authors:
Emil Lundberg
Petter Lundahl
***/

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
        readToken()
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

    def parseGoal(): Option[Program] =
      parseMainObject() flatMap (main => {
        val classes = parseClassDeclarations()
        eat(EOF) flatMap (_ =>
          if(ctx.reporter.hasErrors)
            None
          else
            Some(Program(main, classes))
        )
      })

    def parseMainObject(): Option[MainObject] =
      eat(OBJECT) flatMap (_ =>
        eatIdentifier() flatMap (id =>
          eatSequence(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE) flatMap (_ => {
            val statements = parseStatements()
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
                    eatIdentifier() flatMap (paramId =>
                      parseType() map (Formal(_, paramId))
                    )
                  } else None) ++: (
                    accumulate(() =>
                      eat(COMMA) flatMap (_ =>
                        eatIdentifier() flatMap (paramId =>
                          parseType() map (Formal(_, paramId))
                        )
                      )
                    ) whilst(() => currentToken is COMMA))

                  eat(RPAREN) flatMap (_ =>
                    parseType() flatMap (returnType =>
                      eatSequence(EQSIGN, LBRACE) flatMap (_ => {
                        val varDeclarations = parseVarDeclarations()
                        val statements = parseStatements()

                        eat(RETURN) flatMap (_ =>
                          parseExpression() flatMap (returnExpression =>
                            eatSequence(SEMICOLON, RBRACE) map (_ =>
                              MethodDecl(returnType, id,
                                parameters, varDeclarations, statements, returnExpression)
                            )
                          )
                        )
                      })
                    )
                  )
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
      def parseVarDeclaration(): Option[VarDecl] =
        eat(VAR) flatMap (_ =>
          eatIdentifier() flatMap (id =>
            parseType() map (tpe => VarDecl(tpe, id)) flatMap (varDecl =>
              eat(SEMICOLON) map (_ => varDecl)
            )
          )
        )

      accumulate(parseVarDeclaration) whilst(() => currentToken is VAR)
    }

    def parseType(): Option[TypeTree] =
      eat(COLON) flatMap (_ =>
        currentToken.kind match {
          case BOOLEAN => eat(BOOLEAN) map (_ => new BooleanType)
          case STRING  => eat(STRING) map (_ =>  new StringType)
          case IDKIND  => eatIdentifier()
          case INT     => eat(INT) flatMap (_ =>
              if(currentToken is LBRACKET) {
                eatSequence(LBRACKET, RBRACKET) map (_ => new IntArrayType)
              } else Some(new IntType)
            )
          case _ => {
            expected(BOOLEAN, STRING, IDKIND, INT)
            None
          }
        }
      )

    def parseStatement(): Option[StatTree] = {

      def parseBlock(): Option[StatTree] =
        eat(LBRACE) flatMap (_ => {
          val statements = parseStatements()
          eat(RBRACE) map (_ =>
            Block(statements)
          )
      })

      def parseIf(): Option[If] =
        eatSequence(IF, LPAREN) flatMap (_ =>
          parseExpression() flatMap (expression =>
            eat(RPAREN) flatMap (_ =>
              parseStatement() map (thenStatement => {
                val elseStatement = if(currentToken is ELSE) {
                  eat(ELSE) flatMap (_ =>
                    parseStatement()
                  )
                } else None

                If(expression, thenStatement, elseStatement)
              })
            )
          )
        )

      def parseWhile(): Option[While] =
        eatSequence(WHILE, LPAREN) flatMap (_ =>
          parseExpression() flatMap (expression =>
            eat(RPAREN) flatMap (_ =>
              parseStatement() map ( doStatement => While(expression, doStatement) )
            )
          )
        )

      def parsePrintln(): Option[Println] =
        eatSequence(PRINTLN, LPAREN) flatMap (_ =>
          parseExpression() flatMap (expression =>
            eatSequence(RPAREN, SEMICOLON) map (_ =>
              Println(expression)
            )
          )
        )

      def parseAssignment(): Option[StatTree] = eatIdentifier() flatMap (assignId =>
        currentToken.kind match {
          case EQSIGN   => eat(EQSIGN) flatMap (_ =>
            parseExpression() flatMap (expression =>
              eat(SEMICOLON) map (_ =>
                Assign(assignId, expression)
              )
            )
          )
          case LBRACKET => eat(LBRACKET) flatMap (_ =>
            parseExpression() flatMap (index =>
              eatSequence(RBRACKET, EQSIGN) flatMap (_ =>
                parseExpression() flatMap (value =>
                  eat(SEMICOLON) map (_ => ArrayAssign(assignId, index, value))
                )
              )
            )
          )
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
          expected(BEGIN_STATEMENT:_*)
          None
        }
      }
    }

    def parseStatements(): List[StatTree] =
      accumulate(parseStatement) whilst(() => currentToken is (BEGIN_STATEMENT:_*))

    def parseMethodCall(obj: ExprTree): Option[ExprTree] =
      eatIdentifier() flatMap (identifier =>
        eat(LPAREN) flatMap (_ => {
          val args = (if(currentToken is (BEGIN_EXPRESSION:_*)) {
            parseExpression()
          } else None) ++: (
            accumulate(() =>
              eat(COMMA) flatMap (_ => parseExpression())
            ) whilst(() => currentToken is COMMA))

          eat(RPAREN) map (_ => MethodCall(obj, identifier, args))
        })
      )

    def parseNegation(): Option[ExprTree] = {

      def parseNew(): Option[ExprTree] =
        eat(NEW) flatMap (_ =>
          if(currentToken is IDKIND) {
            eatIdentifier() flatMap (id =>
              eatSequence(LPAREN, RPAREN) map (_ => New(id))
            )
          } else {
            eatSequence(INT, LBRACKET) flatMap (_ =>
              parseExpression() flatMap (expression =>
                eat(RBRACKET) map (_ => NewIntArray(expression))
              )
            )
          }
        )

      def maybeParseDot(expression: ExprTree): Option[ExprTree] =
        if(currentToken is DOT) {
          eat(DOT) flatMap (_ =>
            if(currentToken is LENGTH) {
              eat(LENGTH) flatMap (_ =>
                maybeParseDot(ArrayLength(expression))
              )
            } else {
              parseMethodCall(expression) flatMap maybeParseDot
            }
          )
        } else Some(expression)

      def maybeParseArrayRead(expression: ExprTree): Option[ExprTree] =
        if(currentToken is LBRACKET) {
          eat(LBRACKET) flatMap (_ =>
            parseExpression() flatMap (index =>
              eat(RBRACKET) map (_ => ArrayRead(expression, index))
            )
          )
        } else Some(expression)

      def parseExpressionBase(): Option[ExprTree] = currentToken match {
        case INTLIT(value) => eat(INTLITKIND) map (_ => IntLit(value))
        case STRLIT(value) => eat(STRLITKIND) map (_ => StringLit(value))
        case ID(value)     => eat(IDKIND)     map (_ => Identifier(value))
        case _             => currentToken.kind match {
          case TRUE        => eat(TRUE)  map (_ => new True)
          case FALSE       => eat(FALSE) map (_ => new False)
          case THIS        => eat(THIS)  map (_ => new This)
          case NEW         => parseNew()
          case BANG        => eat(BANG) flatMap (_ => parseNegation()) map Not
          case LPAREN      => eat(LPAREN) flatMap(_ =>
            parseExpression() flatMap (expression =>
              eat(RPAREN) map (_ => expression)
            )
          )
          case _           => {
            expected(BEGIN_EXPRESSION:_*)
            None
          }
        }
      }

      parseExpressionBase() flatMap maybeParseDot flatMap maybeParseArrayRead
    }

    def maybeParseRightFactor(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case TIMES => eat(TIMES) flatMap (_ =>
        parseNegation() flatMap (rhs => maybeParseRightFactor(Times(lhs, rhs)))
      )
      case DIV   => eat(DIV) flatMap (_ =>
        parseNegation() flatMap (rhs => maybeParseRightFactor(Div(lhs, rhs)))
      )
      case _     => Some(lhs)
    }
    def parseProduct(): Option[ExprTree] = parseNegation() flatMap maybeParseRightFactor

    def maybeParseRightTerm(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case PLUS  => eat(PLUS) flatMap (_ =>
        parseProduct() flatMap (rhs => maybeParseRightTerm(Plus(lhs, rhs)))
      )
      case MINUS => eat(MINUS) flatMap (_ =>
        parseProduct() flatMap (rhs => maybeParseRightTerm(Minus(lhs, rhs)))
      )
      case _     => Some(lhs)
    }
    def parseSum(): Option[ExprTree] = parseProduct() flatMap maybeParseRightTerm

    def maybeParseRightComparee(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case LESSTHAN => eat(LESSTHAN) flatMap (_ =>
        parseSum() flatMap (rhs => maybeParseRightComparee(LessThan(lhs, rhs)))
      )
      case EQUALS   => eat(EQUALS) flatMap (_ =>
        parseSum() flatMap (rhs => maybeParseRightComparee(Equals(lhs, rhs)))
      )
      case _        => Some(lhs)
    }
    def parseComparison(): Option[ExprTree] = parseSum() flatMap maybeParseRightComparee

    def maybeParseRightAnd(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case AND => eat(AND) flatMap (_ =>
        parseComparison() flatMap (rhs => maybeParseRightAnd(And(lhs, rhs)))
      )
      case _   => Some(lhs)
    }
    def parseAnd(): Option[ExprTree] = parseComparison() flatMap maybeParseRightAnd

    def maybeParseRightOr(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case OR  => eat(OR) flatMap (_ =>
        parseAnd() flatMap (rhs => maybeParseRightOr(Or(lhs, rhs)))
      )
      case _   => Some(lhs)
    }
    def parseExpression(): Option[ExprTree] = parseAnd() flatMap maybeParseRightOr

    readToken()
    parseGoal()
  }
}
