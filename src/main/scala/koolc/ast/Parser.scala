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

    def read[T](kind: TokenKind): ((Token => Option[T]) => Option[T]) = {
      sealed class ReadTokenResult[T](val token: Option[Token]) extends ((Token => Option[T]) => Option[T]) {
        def apply(thenn: (Token => Option[T])): Option[T] = token flatMap thenn
      }
      new ReadTokenResult(
        if (currentToken.kind == kind) {
          val current = Some(currentToken)
          readToken
          current
        } else {
          expected(kind)
          readToken()
          None
        }
      )
    }

    def eat[T](kind: TokenKind, moreKinds: TokenKind*): (Option[T] => Option[T]) = {
      sealed class EatTokenResult[T](val token: Option[Token]) extends (Option[T] => Option[T]) {
        def apply(thenn: Option[T]): Option[T] = token flatMap (_ => thenn)
      }

      moreKinds.toList match {
        case Nil          => new EatTokenResult(read(kind) { t: Token => Some(t) })
        case head :: tail => {
          read(kind) (_ => None)
          eat(head, tail:_*)
        }
      }
    }

    def readIdentifier[T](thenn: Identifier => Option[T] = Some[Identifier](_)): Option[T] =
      read(IDKIND) { token => token match {
          case ID(value) => Some(Identifier(value).setPos(token))
          case _         => None
        }
      } orElse {
        expected(IDKIND)
        None
      } flatMap thenn

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
            Some(Program(main, classes).setPos(main))
        }
      })

    def parseMainObject[T](thenn: MainObject => Option[T] = Some[MainObject](_)): Option[T] =
      read(OBJECT) { objectToken =>
        readIdentifier(id =>
          eat(LBRACE, DEF, MAIN, LPAREN, RPAREN, COLON, UNIT, EQSIGN, LBRACE) {
            val statements = parseStatements()
            eat(RBRACE, RBRACE) {
              Some(MainObject(id, statements).setPos(objectToken))
            }
          }
        )
      } flatMap thenn

    def parseClassDeclarations(): List[ClassDecl] = {
      def parseClassDeclaration(): Option[ClassDecl] = {
        def parseMethodDeclarations(): List[MethodDecl] = {
          def parseMethodDeclaration(): Option[MethodDecl] =
            read(DEF) { defToken =>
              readIdentifier(id => {
                  val templateList = parseTemplateList()
                  eat(LPAREN) {
                    val parameters: List[Formal] =
                      if(currentToken is IDKIND) {
                        readIdentifier(paramId => {
                          eat(COLON)
                          parseType(tpe => Some(Formal(tpe, paramId).setPos(paramId)))
                          }
                        ) map { firstParam =>
                          firstParam +: {
                            accumulate {
                              eat(COMMA) {
                                readIdentifier(paramId =>{
                                  eat(COLON)
                                  parseType(tpe => Some(Formal(tpe, paramId).setPos(paramId)))
                                  }
                                )
                              }
                            } whilst { currentToken is COMMA }
                          }
                        } getOrElse Nil
                      } else Nil

                    eat(RPAREN) {
                      eat(COLON)
                      parseType(returnType =>
                        eat(EQSIGN, LBRACE) {
                          val varDeclarations = parseVarDeclarations()
                          val statements = parseStatements()

                          eat(RETURN) {
                            parseExpression(returnExpression =>
                              eat(SEMICOLON, RBRACE) {
                                Some(MethodDecl(returnType, id,
                                  parameters, varDeclarations, statements, returnExpression, templateList).setPos(defToken)
                                )
                              }
                            )
                          }
                        }
                      )
                    }
                  }
                }
              )
            }

          accumulate { parseMethodDeclaration() } whilst { currentToken is DEF }
        }

        read(CLASS) { classToken =>
          readIdentifier(id => {
            val templateList = parseTemplateList()
            val parentClass = if(currentToken is EXTENDS) {
              eat(EXTENDS) { readIdentifier() }
            } else None
            eat(LBRACE) {
              val vars = parseVarDeclarations()
              val methods = parseMethodDeclarations()
              eat(RBRACE) {
                Some(ClassDecl(id, parentClass, vars, methods, templateList).setPos(classToken))
              }
            }
          })
        }
      }

      accumulate { parseClassDeclaration() } whilst { currentToken is CLASS }
    }

    def parseTemplateArg(): List[TypeTree] = {
      def parseTemplateType(): Option[TypeTree] =
        parseType(id =>{
          if(currentToken is COMMA)
            eat(COMMA)
          Some(id.setPos(id))
          }
        )
      if(currentToken is LANGEL){
        eat(LANGEL)
        val result = accumulate { parseTemplateType() } whilst { currentToken is IDKIND }
        eat(RANGEL)
        result
      }else Nil

    }

    def parseTemplateList(): List[Identifier] = {
      def parseTemplateId(): Option[Identifier] =
        readIdentifier(id =>{
          if(currentToken is COMMA)
            eat(COMMA)
          Some(id.setPos(id))
          }
        )
      if(currentToken is LANGEL){
        eat(LANGEL)
        val result = accumulate { parseTemplateId() } whilst { currentToken is IDKIND }
        eat(RANGEL)
        result
      }else Nil
    }

    def parseVarDeclarations(): List[VarDecl] = {
      def parseVarDeclaration(): Option[VarDecl] =
        read(VAR) { varToken =>
          readIdentifier(id => {
              eat(COLON)
              parseType(tpe =>
                eat(SEMICOLON) {
                  Some(VarDecl(tpe, id).setPos(varToken))
                }
              )
            }
          )
        }

      accumulate { parseVarDeclaration() } whilst { currentToken is VAR }
    }

    def parseType[T](thenn: TypeTree => Option[T] = Some[TypeTree](_)): Option[T] =
      {
        currentToken.kind match {
          case BOOLEAN => read(BOOLEAN) { token => Some(BooleanType().setPos(token)) }
          case STRING  => read(STRING)  { token => Some(StringType().setPos(token))  }
          case IDKIND  => {
            readIdentifier() match {
              case None => None;
              case Some(id) => Some(Identifier(id.value,parseTemplateArg()));
            }
          }
          case INT     => read(INT) { intToken =>
              if(currentToken is LBRACKET) {
                eat(LBRACKET, RBRACKET) { Some(IntArrayType().setPos(intToken)) }
              } else Some(IntType().setPos(intToken))
            }
          case _ => {
            expected(BOOLEAN, STRING, IDKIND, INT)
            None
          }
        }
      } flatMap thenn

    def parseStatement[T](thenn: StatTree => Option[T] = Some[StatTree](_)): Option[T] = {

      def parseBlock(): Option[StatTree] =
        read(LBRACE) { braceToken =>
          val statements = parseStatements()
          eat(RBRACE) {
            Some(Block(statements).setPos(braceToken))
          }
        }

      def parseIf(): Option[If] =
        read(IF) { ifToken =>
          eat(LPAREN) {
            parseExpression(expression =>
              eat(RPAREN) {
                parseStatement(thenStatement => {
                  val elseStatement = if(currentToken is ELSE) {
                    eat(ELSE) {
                      parseStatement()
                    }
                  } else None

                  Some(If(expression, thenStatement, elseStatement).setPos(ifToken))
                })
              }
            )
          }
        }

      def parseWhile(): Option[While] =
        read(WHILE) { whileToken =>
          eat(LPAREN) {
            parseExpression(expression =>
              eat(RPAREN) {
                parseStatement(doStatement => Some(While(expression, doStatement).setPos(whileToken)))
              }
            )
          }
        }

      def parsePrintln(): Option[Println] =
        read(PRINTLN) { printlnToken =>
          eat(LPAREN) {
            parseExpression(expression =>
              eat(RPAREN, SEMICOLON) {
                Some(Println(expression).setPos(printlnToken))
              }
            )
          }
        }

      def parseAssignment(): Option[StatTree] = readIdentifier(assignId =>
        currentToken.kind match {
          case EQSIGN   => eat(EQSIGN) {
            parseExpression(expression =>
              eat(SEMICOLON) {
                Some(Assign(assignId, expression).setPos(assignId))
              }
            )
          }
          case LBRACKET => eat(LBRACKET) {
            parseExpression(index =>
              eat(RBRACKET, EQSIGN) {
                parseExpression(value =>
                  eat(SEMICOLON) { Some(ArrayAssign(assignId, index, value).setPos(assignId)) }
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
      accumulate { parseStatement() } whilst { currentToken is (BEGIN_STATEMENT:_*) }

    def parseExpression[T](thenn: ExprTree => Option[T] = Some[ExprTree](_)): Option[T] = {

      def parseMethodCall(obj: ExprTree): Option[ExprTree] =
        readIdentifier(identifier =>
          eat(LPAREN) {
            val args: List[ExprTree] =
              if(currentToken is (BEGIN_EXPRESSION:_*)) {
                parseExpression() map { firstArgument =>
                  firstArgument +: {
                    accumulate { eat(COMMA) { parseExpression() } } whilst { currentToken is COMMA }
                  }
                } getOrElse Nil
              } else Nil

            eat(RPAREN) { Some(MethodCall(obj, identifier, args).setPos(obj)) }
          }
        )

      def parseNegation(thenn: ExprTree => Option[ExprTree] = Some(_)): Option[ExprTree] = {
        def parseNew(): Option[ExprTree] =
          read(NEW) { newToken =>
            if(currentToken is IDKIND) {
              readIdentifier(id =>
                eat(LPAREN, RPAREN) { Some(New(id).setPos(newToken)) }
              )
            } else {
              eat(INT, LBRACKET) {
                parseExpression(expression =>
                  eat(RBRACKET) { Some(NewIntArray(expression).setPos(newToken)) }
                )
              }
            }
          }

        def maybeParseDot(expression: ExprTree): Option[ExprTree] =
          if(currentToken is DOT) {
            eat(DOT) {
              if(currentToken is LENGTH) {
                eat(LENGTH) {
                  maybeParseDot(ArrayLength(expression).setPos(expression))
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
                eat(RBRACKET) { Some(ArrayRead(expression, index).setPos(expression)) }
              )
            }
          } else Some(expression)

        def parseExpressionBase(): Option[ExprTree] = currentToken match {
          case INTLIT(value) => read(INTLITKIND) { token => Some(IntLit(value)   .setPos(token)) }
          case STRLIT(value) => read(STRLITKIND) { token => Some(StringLit(value).setPos(token)) }
          case ID(value)     => readIdentifier()
          case _             => currentToken.kind match {
            case TRUE        => read(TRUE)       { token => Some(True()          .setPos(token)) }
            case FALSE       => read(FALSE)      { token => Some(False()         .setPos(token)) }
            case THIS        => read(THIS)       { token => Some(This()          .setPos(token)) }
            case NEW         => parseNew()
            case BANG        => read(BANG) { bangToken =>
              parseNegation(negation => Some(Not(negation).setPos(bangToken)))
            }
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

        parseExpressionBase() flatMap maybeParseDot flatMap maybeParseArrayRead flatMap thenn
      }

      def maybeParseRightFactor(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
        case TIMES => eat(TIMES) { parseNegation(rhs => maybeParseRightFactor(Times(lhs, rhs).setPos(lhs))) }
        case DIV   => eat(DIV)   { parseNegation(rhs => maybeParseRightFactor(Div(lhs, rhs).setPos(lhs)))   }
        case _     => Some(lhs)
      }
      def parseProduct(thenn: ExprTree => Option[ExprTree] = Some(_)): Option[ExprTree] =
        parseNegation() flatMap maybeParseRightFactor flatMap thenn

      def maybeParseRightTerm(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
        case PLUS  => eat(PLUS)  { parseProduct(rhs => maybeParseRightTerm(Plus(lhs, rhs).setPos(lhs)))  }
        case MINUS => eat(MINUS) { parseProduct(rhs => maybeParseRightTerm(Minus(lhs, rhs).setPos(lhs))) }
        case _     => Some(lhs)
      }
      def parseSum(thenn: ExprTree => Option[ExprTree] = Some(_)): Option[ExprTree] =
        parseProduct() flatMap maybeParseRightTerm flatMap thenn

      def maybeParseRightComparee(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
        case LESSTHAN => eat(LESSTHAN) { parseSum(rhs => maybeParseRightComparee(LessThan(lhs, rhs).setPos(lhs))) }
        case EQUALS   => eat(EQUALS)   { parseSum(rhs => maybeParseRightComparee(Equals(lhs, rhs).setPos(lhs)))   }
        case _        => Some(lhs)
      }
      def parseComparison(thenn: ExprTree => Option[ExprTree] = Some(_)): Option[ExprTree] =
        parseSum() flatMap maybeParseRightComparee flatMap thenn

      def maybeParseRightAnd(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
        case AND => eat(AND) { parseComparison(rhs => maybeParseRightAnd(And(lhs, rhs).setPos(lhs))) }
        case _   => Some(lhs)
      }
      def parseAnd(thenn: ExprTree => Option[ExprTree] = Some(_)): Option[ExprTree] =
        parseComparison() flatMap maybeParseRightAnd flatMap thenn

      def maybeParseRightOr(lhs: ExprTree): Option[ExprTree] = currentToken.kind match {
        case OR  => eat(OR) { parseAnd(rhs => maybeParseRightOr(Or(lhs, rhs).setPos(lhs))) }
        case _   => Some(lhs)
      }

      parseAnd() flatMap maybeParseRightOr flatMap thenn
    }

    readToken()
    parseGoal()
  }
}
