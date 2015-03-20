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

        var statements = new ListBuffer[StatTree]
        while(currentToken isnt RBRACE) {
          statements ++= parseStatement()
        }

        eatSequence(RBRACE, RBRACE)

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
              var parameters = new ListBuffer[Formal]
              while(currentToken isnt RPAREN) {
                if(parameters.length > 0) eat(COMMA)
                parameters ++= eatIdentifier() map ( paramId => Formal(parseType(), paramId) )
              }
              eat(RPAREN)
              val returnType = parseType()
              eatSequence(EQSIGN, LBRACE)

              val varDeclarations = parseVarDeclarations()

              var statements = new ListBuffer[StatTree]
              while(currentToken isnt RETURN) {
                statements ++= parseStatement()
              }

              eat(RETURN)
              val returnExpression = parseExpression()
              eatSequence(SEMICOLON, RBRACE)
              MethodDecl(returnType, id,
                parameters.toList, varDeclarations.toList, statements.toList, returnExpression)
            })
          }

          var methods = new ListBuffer[MethodDecl]
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

      var classes = new ListBuffer[ClassDecl]
      while(currentToken is CLASS) {
        classes ++= parseClassDeclaration()
      }
      classes.toList
    }

    def parseVarDeclarations(): List[VarDecl] = {
      def parseVarDeclaration(): Option[VarDecl] = {
        eat(VAR)
        eatIdentifier() map (id => {
          val variable = VarDecl(parseType(), id)
          eat(SEMICOLON)
          variable
        })
      }

      var variables = new ListBuffer[VarDecl]
      while(currentToken is VAR) {
        variables ++= parseVarDeclaration()
      }
      variables.toList
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
        var statements = new ListBuffer[StatTree]
        while(currentToken isnt RBRACE) {
          statements ++= parseStatement()
        }
        eat(RBRACE)
        Some(Block(statements.toList))
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
        val expression = parseExpression()
        eatSequence(RPAREN, SEMICOLON)
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
            eatSequence(RBRACKET, EQSIGN)
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
          val result = New(eatIdentifier().get)
          eatSequence(LPAREN, RPAREN)
          result
        } else {
          eatSequence(INT, LBRACKET)
          val result = NewIntArray(parseExpression())
          eat(RBRACKET)
          result
        }

      }

      def parseDot(expression: ExprTree): ExprTree = {
        if(currentToken is LENGTH) {
          eat(LENGTH);
          return ArrayLength(expression);
        } else {
          return parseMethodCall(expression);
        }
      }

      var negation: ExprTree = null;
      currentToken match {
        case INTLIT(value) => { eat(INTLITKIND); negation = IntLit(value) }
        case STRLIT(value) => { eat(STRLITKIND); negation = StringLit(value) }
        case ID(value)     => { eat(IDKIND);     negation = Identifier(value) }
        case _             => currentToken.kind match {
          case TRUE        => { eat(TRUE); negation = new True }
          case FALSE       => { eat(FALSE); negation = new False }
          case THIS        => { eat(THIS); negation = new This }
          case NEW         => negation = parseNew()
          case BANG        => { eat(BANG); negation = Not(parseNegation()) }
          case LPAREN      => { eat(LPAREN); negation = parseExpression(); eat(RPAREN)}
          case _           => ???
        }
      }
      if(currentToken is DOT) {
        eat(DOT);
        negation = parseDot(negation)
      }
      if(currentToken is LBRACKET) {
        eat(LBRACKET);
        negation = ArrayRead(negation, parseExpression())
        eat(RBRACKET);
      }
      negation
    }

    def parseFactor(): ExprTree = {
      var factor = parseNegation()
      while((currentToken is TIMES) || (currentToken is DIV)) {
        currentToken.kind match {
          case TIMES => { eat(TIMES); factor = Times(factor, parseNegation()) }
          case DIV   => { eat(DIV);   factor = Div(factor, parseNegation()) }
          case _     => ???
        }
      }
      factor
    }

    def parseTerm(): ExprTree = {
      var term = parseFactor()
      while((currentToken is PLUS) || (currentToken is MINUS)) {
        currentToken.kind match {
          case PLUS  => { eat(PLUS);  term = Plus(term, parseFactor()) }
          case MINUS => { eat(MINUS); term = Minus(term, parseFactor()) }
          case _     => ???
        }
      }
      term
    }


    def parseCompare(): ExprTree = {
      var compare = parseTerm()
      while((currentToken is LESSTHAN) || (currentToken is EQUALS)) {
        currentToken.kind match {
          case LESSTHAN => { eat(LESSTHAN); compare = LessThan(compare, parseTerm()) }
          case EQUALS   => { eat(EQUALS);   compare = Equals(compare, parseTerm()) }
          case _        => ???
        }
      }
      compare
    }

    def parseExpression(): ExprTree = {
      var expression = parseCompare()
      while((currentToken is AND) || (currentToken is OR)) {
        currentToken.kind match {
          case AND => { eat(AND); expression = And(expression, parseCompare()) }
          case OR  => { eat(OR);  expression = Or(expression, parseCompare()) }
          case _   => ???
        }
      }
      expression
    }

    readToken()
    parseGoal()
  }
}
