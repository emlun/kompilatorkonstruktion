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

    def eatInternal(kind: TokenKind): Option[Token] = {
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

    def eat[T](kind: TokenKind, moreKinds: TokenKind*): (Option[T] => Option[T]) = {
      sealed class EatTokenResult[T](val token: Option[Token]) extends (Option[T] => Option[T]) {
        def apply(thenn: Option[T]): Option[T] = token flatMap (_ => thenn)
      }

      moreKinds.toList match {
        case Nil          => new EatTokenResult(eatInternal(kind))
        case head :: tail => eatInternal(kind) match {
          case Some(_) => eat(head, tail:_*)
          case None        => (_ => None)
        }
      }
    }

    def eatIdentifier[T](thenn: Identifier => Option[T] = Some[Identifier](_)): Option[T] =
      eatInternal(IDKIND) match {
        case Some(ID(value)) => thenn(new Identifier(value))
        case _               => {
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
      parseMainObject(main => {
        val classes = parseClassDeclarations()
        eat(EOF) {
          if(ctx.reporter.hasErrors)
            None
          else
            Some(Program(main, classes))
        }
      })

    def parseMainObject[T](thenn: MainObject => Option[T] = Some[MainObject](_)): Option[T] =
      eat(OBJECT) {
        eatIdentifier(id =>
          eat(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE) {
            val statements = parseStatements()
            eat(RBRACE, RBRACE) {
              Some(MainObject(id, statements))
            }
          }
        )
      } flatMap thenn

    def parseClassDeclarations(): List[ClassDecl] = {
      def parseClassDeclaration(): Option[ClassDecl] = {
        def parseMethodDeclarations(): List[MethodDecl] = {
          def parseMethodDeclaration(): Option[MethodDecl] =
            eat(DEF) {
              eatIdentifier(id =>
                eat(LPAREN) {
                  val parameters = (if(currentToken is IDKIND) {
                    eatIdentifier(paramId =>
                      parseType(tpe => Some(Formal(tpe, paramId)))
                    )
                  } else None) ++: (
                    accumulate(() =>
                      eat(COMMA) {
                        eatIdentifier(paramId =>
                          parseType(tpe => Some(Formal(tpe, paramId)))
                        )
                      }
                    ) whilst(() => currentToken is COMMA))

                  eat(RPAREN) {
                    parseType(returnType =>
                      eat(EQSIGN, LBRACE) {
                        val varDeclarations = parseVarDeclarations()
                        val statements = parseStatements()

                        eat(RETURN) {
                          parseExpression(returnExpression =>
                            eat(SEMICOLON, RBRACE) {
                              Some(MethodDecl(returnType, id,
                                parameters, varDeclarations, statements, returnExpression)
                              )
                            }
                          )
                        }
                      }
                    )
                  }
                }
              )
            }

          accumulate(parseMethodDeclaration) whilst(() => currentToken is DEF)
        }

        eat(CLASS) {
          eatIdentifier(id => {
            val parentClass = if(currentToken is EXTENDS) {
              eat(EXTENDS) { eatIdentifier() }
            } else None
            eat(LBRACE) {
              val vars = parseVarDeclarations()
              val methods = parseMethodDeclarations()
              eat(RBRACE) {
                Some(ClassDecl(id, parentClass, vars, methods))
              }
            }
          })
        }
      }

      accumulate(parseClassDeclaration) whilst(() => currentToken is CLASS)
    }

    def parseVarDeclarations(): List[VarDecl] = {
      def parseVarDeclaration(): Option[VarDecl] =
        eat(VAR) {
          eatIdentifier(id =>
            parseType(tpe => Some(VarDecl(tpe, id))) flatMap (varDecl =>
              eat(SEMICOLON) {
                Some(varDecl)
              }
            )
          )
        }

      accumulate(parseVarDeclaration) whilst(() => currentToken is VAR)
    }

    def parseType[T](thenn: TypeTree => Option[T] = Some[TypeTree](_)): Option[T] =
      eat(COLON) {
        currentToken.kind match {
          case BOOLEAN => eat(BOOLEAN) { Some(new BooleanType) }
          case STRING  => eat(STRING) { Some(new StringType) }
          case IDKIND  => eatIdentifier()
          case INT     => eat(INT) {
              if(currentToken is LBRACKET) {
                eat(LBRACKET, RBRACKET) { Some(new IntArrayType) }
              } else Some(new IntType)
            }
          case _ => {
            expected(BOOLEAN, STRING, IDKIND, INT)
            None
          }
        }
      } flatMap thenn

    def parseStatement[T](thenn: StatTree => Option[T] = Some[StatTree](_)): Option[T] = {

      def parseBlock(): Option[StatTree] =
        eat(LBRACE) {
          val statements = parseStatements()
          eat(RBRACE) {
            Some(Block(statements))
          }
        }

      def parseIf(): Option[If] =
        eat(IF, LPAREN) {
          parseExpression(expression =>
            eat(RPAREN) {
              parseStatement(thenStatement => {
                val elseStatement = if(currentToken is ELSE) {
                  eat(ELSE) {
                    parseStatement()
                  }
                } else None

                Some(If(expression, thenStatement, elseStatement))
              })
            }
          )
        }

      def parseWhile(): Option[While] =
        eat(WHILE, LPAREN) {
          parseExpression(expression =>
            eat(RPAREN) {
              parseStatement(doStatement => Some(While(expression, doStatement)))
            }
          )
        }

      def parsePrintln(): Option[Println] =
        eat(PRINTLN, LPAREN) {
          parseExpression(expression =>
            eat(RPAREN, SEMICOLON) {
              Some(Println(expression))
            }
          )
        }

      def parseAssignment(): Option[StatTree] = eatIdentifier(assignId =>
        currentToken.kind match {
          case EQSIGN   => eat(EQSIGN) {
            parseExpression(expression =>
              eat(SEMICOLON) {
                Some(Assign(assignId, expression))
              }
            )
          }
          case LBRACKET => eat(LBRACKET) {
            parseExpression(index =>
              eat(RBRACKET, EQSIGN) {
                parseExpression(value =>
                  eat(SEMICOLON) { Some(ArrayAssign(assignId, index, value)) }
                )
              }
            )
          }
          case _        => {
            expected(EQSIGN, LBRACKET)
            None
          }
        }
      )

      (currentToken.kind match {
        case LBRACE  => parseBlock()
        case IF      => parseIf()
        case WHILE   => parseWhile()
        case PRINTLN => parsePrintln()
        case IDKIND  => parseAssignment()
        case _       => {
          expected(BEGIN_STATEMENT:_*)
          None
        }
      }) flatMap thenn
    }

    def parseStatements(): List[StatTree] =
      accumulate(() => parseStatement()) whilst(() => currentToken is (BEGIN_STATEMENT:_*))

    def parseMethodCall(obj: ExprTree): Option[ExprTree] =
      eatIdentifier(identifier =>
        eat(LPAREN) {
          val args = (if(currentToken is (BEGIN_EXPRESSION:_*)) {
            parseExpression()
          } else None) ++: (
            accumulate(() =>
              eat(COMMA) { parseExpression() }
            ) whilst(() => currentToken is COMMA))

          eat(RPAREN) { Some(MethodCall(obj, identifier, args)) }
        }
      )

    def parseNegation(): Option[ExprTree] = {

      def parseNew(): Option[ExprTree] =
        eat(NEW) {
          if(currentToken is IDKIND) {
            eatIdentifier(id =>
              eat(LPAREN, RPAREN) { Some(New(id)) }
            )
          } else {
            eat(INT, LBRACKET) {
              parseExpression(expression =>
                eat(RBRACKET) { Some(NewIntArray(expression)) }
              )
            }
          }
        }

      def maybeParseDot(expression: ExprTree): Option[ExprTree] =
        if(currentToken is DOT) {
          eat(DOT) {
            if(currentToken is LENGTH) {
              eat(LENGTH) {
                maybeParseDot(ArrayLength(expression))
              }
            } else {
              parseMethodCall(expression) flatMap maybeParseDot
            }
          }
        } else Some(expression)

      def maybeParseArrayRead(expression: ExprTree): Option[ExprTree] =
        if(currentToken is LBRACKET) {
          eat(LBRACKET) {
            parseExpression(index =>
              eat(RBRACKET) { Some(ArrayRead(expression, index)) }
            )
          }
        } else Some(expression)

      def parseExpressionBase(): Option[ExprTree] = currentToken match {
        case INTLIT(value) => eat(INTLITKIND) { Some(IntLit(value))     }
        case STRLIT(value) => eat(STRLITKIND) { Some(StringLit(value))  }
        case ID(value)     => eat(IDKIND)     { Some(Identifier(value)) }
        case _             => currentToken.kind match {
          case TRUE        => eat(TRUE)  { Some(new True)  }
          case FALSE       => eat(FALSE) { Some(new False) }
          case THIS        => eat(THIS)  { Some(new This)  }
          case NEW         => parseNew()
          case BANG        => eat(BANG) { parseNegation() map Not }
          case LPAREN      => eat(LPAREN) {
            parseExpression(expression =>
              eat(RPAREN) { Some(expression) }
            )
          }
          case _           => {
            expected(BEGIN_EXPRESSION:_*)
            None
          }
        }
      }

      parseExpressionBase() flatMap maybeParseDot flatMap maybeParseArrayRead
    }

    def maybeParseRightFactor(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case TIMES => eat(TIMES) { parseNegation() flatMap (rhs => maybeParseRightFactor(Times(lhs, rhs))) }
      case DIV   => eat(DIV)   { parseNegation() flatMap (rhs => maybeParseRightFactor(Div(lhs, rhs)))   }
      case _     => Some(lhs)
    }
    def parseProduct(): Option[ExprTree] = parseNegation() flatMap maybeParseRightFactor

    def maybeParseRightTerm(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case PLUS  => eat(PLUS)  { parseProduct() flatMap (rhs => maybeParseRightTerm(Plus(lhs, rhs)))  }
      case MINUS => eat(MINUS) { parseProduct() flatMap (rhs => maybeParseRightTerm(Minus(lhs, rhs))) }
      case _     => Some(lhs)
    }
    def parseSum(): Option[ExprTree] = parseProduct() flatMap maybeParseRightTerm

    def maybeParseRightComparee(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case LESSTHAN => eat(LESSTHAN) { parseSum() flatMap (rhs => maybeParseRightComparee(LessThan(lhs, rhs))) }
      case EQUALS   => eat(EQUALS)   { parseSum() flatMap (rhs => maybeParseRightComparee(Equals(lhs, rhs)))   }
      case _        => Some(lhs)
    }
    def parseComparison(): Option[ExprTree] = parseSum() flatMap maybeParseRightComparee

    def maybeParseRightAnd(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case AND => eat(AND) { parseComparison() flatMap (rhs => maybeParseRightAnd(And(lhs, rhs))) }
      case _   => Some(lhs)
    }
    def parseAnd(): Option[ExprTree] = parseComparison() flatMap maybeParseRightAnd

    def maybeParseRightOr(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
      case OR  => eat(OR) { parseAnd() flatMap (rhs => maybeParseRightOr(Or(lhs, rhs))) }
      case _   => Some(lhs)
    }
    def parseExpression[T](thenn: ExprTree => Option[T] = Some[ExprTree](_)): Option[T] =
      parseAnd() flatMap maybeParseRightOr flatMap thenn

    readToken()
    parseGoal()
  }
}
