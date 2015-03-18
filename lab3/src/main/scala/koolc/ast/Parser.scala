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
        while(currentToken isnt RBRACE) {
          statements ++= parseStatement()
        }

        eat(RBRACE)
        eat(RBRACE)

        MainObject(id, statements.toList)
      })
    }

    def parseClassDeclarations(): List[ClassDecl] = {
      def parseClassDeclaration(): Option[ClassDecl] = {
        def parseMethodDeclarations(): List[MethodDecl] = {
          def parseMethodDeclaration(): Option[MethodDecl] = {
            eat(DEF)
            eatIdentifier() map (id => {
              eat(LPAREN)
              val parameters = new ListBuffer[Formal]
              while(currentToken isnt RPAREN) {
                if(parameters.length > 0) eat(COMMA)
                parameters ++= eatIdentifier() map ( paramId => Formal(parseType(), paramId) )
              }
              eat(RPAREN)
              val returnType = parseType()
              eat(EQSIGN)
              eat(LBRACE)

              val varDeclarations = parseVarDeclarations()

              val statements = new ListBuffer[StatTree]
              while(currentToken isnt RETURN) {
                statements ++= parseStatement()
              }

              eat(RETURN)
              val returnExpression = parseExpression()
              eat(SEMICOLON)
              eat(RBRACE)
              MethodDecl(returnType, id,
                parameters.toList, varDeclarations.toList, statements.toList, returnExpression)
            })
          }

          val methods = new ListBuffer[MethodDecl]
          while(currentToken is DEF) {
            methods ++= parseMethodDeclaration()
          }
          methods.toList
        }

        eat(CLASS)
        eatIdentifier() map (id => {
          val parentClass = if(currentToken is EXTENDS) { eat(EXTENDS); eatIdentifier() } else None
          eat(LBRACE);
          val classDeclaration = ClassDecl(id, parentClass, parseVarDeclarations(), parseMethodDeclarations())
          eat(RBRACE);
          classDeclaration
        }) orElse None
      }

      val classes = new ListBuffer[ClassDecl]
      while(currentToken is CLASS) {
        classes ++= parseClassDeclaration()
      }
      classes.toList
    }

    def parseVarDeclarations(): List[VarDecl] = ???

    def parseType(): TypeTree = {
      eat(COLON)
      ???
    }

    def parseStatement(): Option[StatTree] = {

      def parseBlock(): Option[StatTree] = {
        eat(LBRACE)
        val statements = new ListBuffer[StatTree]
        while(currentToken isnt RBRACE) {
          statements ++= parseStatement()
        }
        eat(RBRACE)
        Some(Block(statements.toList))
      }

      def parseIf(): Option[If] = ???
      def parseWhile(): Option[While] = ???

      def parsePrintln(): Option[Println] = {
        eat(PRINTLN)
        eat(LPAREN)
        val expression = parseExpression()
        eat(RPAREN)
        eat(SEMICOLON)
        Some(Println(expression))
      }

      def parseAssignment(): Option[StatTree] = eatIdentifier() flatMap (assignId =>
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
      )

      currentToken.kind match {
        case LPAREN  => parseBlock()
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
      var identifier = eatIdentifier().get;
      eat(LPAREN);
      var args = new ListBuffer[ExprTree];
      while(currentToken.kind != RPAREN)
      {
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
        if(currentToken.kind == IDKIND) {
          val result = new New(eatIdentifier().get)
          eat(LPAREN)
          eat(RPAREN)
          result
        }
        else {
          eat(INT)
          eat(LBRACKET)
          val result = new NewIntArray(parseExpression())
          eat(RBRACKET)
          result
        }
        
      }
      
      def parseDot(expression: ExprTree): ExprTree = {
        if(currentToken is LENGTH)
          return new ArrayLength(expression);
        else
        {
          return parseMethodCall(expression);
        }
      }
      
      var negation: ExprTree = null;
      currentToken match {
        case INTLIT(value) => { eat(INTLITKIND); negation = new IntLit(value) }
        case STRLIT(value) => { eat(STRLITKIND); negation = new StringLit(value) }
        case ID(value)     => { eat(IDKIND);     negation = new Identifier(value) }
        case _             => currentToken.kind match {
          case TRUE        => { eat(TRUE); negation = new True }
          case FALSE       => { eat(FALSE); negation = new False }
          case THIS        => { eat(THIS); negation = new This }
          case NEW         => negation = parseNew()
          case BANG        => { eat(BANG); negation = new Not(parseNegation()) }
          case LPAREN      => { eat(LPAREN); negation = parseExpression(); eat(RPAREN)}
          case _           => ???
        }
      }
      if(currentToken.kind == DOT)
      {
        eat(DOT);
        negation = parseDot(negation)
      }
      if(currentToken.kind == LBRACKET)
      {
        eat(LBRACKET);
        negation = new ArrayRead(negation, parseExpression())
        eat(RBRACKET);
      }
      negation
    }

    def parseFactor(): ExprTree = {
      var factor = parseNegation()
      while(currentToken.kind == TIMES || currentToken.kind == DIV)
      {
        currentToken.kind match {
          case TIMES        => { eat(TIMES); 
                                factor = new Times(factor, parseNegation() )}
          case DIV        => { eat(DIV); 
                                 factor = new Div(factor, parseNegation() )}
          case _           => ???
        }
      }
      factor
    }

    def parseTerm(): ExprTree = {
      var term = parseFactor()
      while((currentToken is PLUS) || (currentToken is MINUS))
      {
        currentToken.kind match {
          case PLUS        => { eat(PLUS); 
                                term = new Plus(term, parseTerm() )}
          case MINUS        => { eat(MINUS); 
                                 term = new Minus(term, parseTerm() )}
          case _           => ???
        }
      }
      term
    }
    
    
    def parseCompare(): ExprTree = {
      var compare = parseTerm()
      while((currentToken is LESSTHAN) || (currentToken is EQUALS))
      {
        currentToken.kind match {
          case LESSTHAN        => { eat(LESSTHAN); 
                                compare = new LessThan(compare, parseCompare() )}
          case EQUALS        => { eat(EQUALS); 
                                 compare = new Equals(compare, parseCompare() )}
          case _           => ???
        }
      }
      compare
    }
    
    def parseExpression(): ExprTree = {
      var expression = parseCompare()
      while((currentToken is AND) || (currentToken is OR))
      {
        currentToken.kind match {
          case AND        => { eat(AND); 
                                expression = new And(expression, parseExpression() )}
          case OR        => { eat(OR); 
                                 expression = new Or(expression, parseExpression() )}
          case _           => ???
        }
      }
      expression
    }

    readToken()
    parseGoal()
  }
}
