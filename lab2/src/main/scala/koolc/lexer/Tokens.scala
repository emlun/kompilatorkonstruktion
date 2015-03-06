package koolc
package lexer

import utils._

sealed class Token(val kind: TokenKind) extends Positioned {
  override def toString = kind.toString
}

sealed trait TokenKind;

object Tokens {

  object STRLITKIND extends TokenKind {
    override def toString = "string literal"
  }

  object INTLITKIND extends TokenKind {
    override def toString = "integer literal"
  }

  object IDKIND extends TokenKind {
    override def toString = "identifier"
  }

  object Kinded {
    def unapply(t: Token): Option[TokenKind] = {
      Some(t.kind)
    }
  }

  case object BAD extends TokenKind         // represents incorrect tokens.
  case object EOF extends TokenKind
  case object COLON extends TokenKind       // :
  case object SEMICOLON extends TokenKind   // ;
  case object DOT extends TokenKind         // .
  case object COMMA extends TokenKind       // ,
  case object EQSIGN extends TokenKind      // =
  case object EQUALS extends TokenKind      // ==
  case object BANG extends TokenKind        // !
  case object LPAREN extends TokenKind      // (
  case object RPAREN extends TokenKind      // )
  case object LBRACKET extends TokenKind    // [
  case object RBRACKET extends TokenKind    // ]
  case object LBRACE extends TokenKind      // {
  case object RBRACE extends TokenKind      // }
  case object AND extends TokenKind         // &&
  case object OR extends TokenKind          // ||
  case object LESSTHAN extends TokenKind    // <
  case object PLUS extends TokenKind        // +
  case object MINUS extends TokenKind       // -
  case object TIMES extends TokenKind       // *
  case object DIV extends TokenKind         // /
  case object OBJECT extends TokenKind      // object
  case object CLASS extends TokenKind       // class
  case object DEF extends TokenKind         // def
  case object VAR extends TokenKind         // var
  case object UNIT extends TokenKind        // unit
  case object MAIN extends TokenKind        // main
  case object STRING extends TokenKind      // string
  case object EXTENDS extends TokenKind     // extends
  case object INT extends TokenKind         // int
  case object BOOLEAN extends TokenKind     // boolean
  case object WHILE extends TokenKind       // while
  case object IF extends TokenKind          // if
  case object ELSE extends TokenKind        // else
  case object RETURN extends TokenKind      // return
  case object LENGTH extends TokenKind      // length
  case object TRUE extends TokenKind        // true
  case object FALSE extends TokenKind       // false
  case object THIS extends TokenKind        // this
  case object NEW extends TokenKind         // new
  case object PRINTLN extends TokenKind     // println

  case object LINECOMMENT extends TokenKind  // //
  case object BLOCKCOMMENT extends TokenKind // /* */

  // Identifiers
  class ID(val value: String) extends Token(IDKIND) {
    override def toString = "ID("+value+")"
  }

  // Integer literals
  class INTLIT(val value: Int) extends Token(INTLITKIND) {
    override def toString = "INT("+value+")"
  }

  // String literals
  class STRLIT(val value: String) extends Token(STRLITKIND) {
    override def toString = "STR("+value+")"
  }

  def isPrefix(prefix: String)(kind: TokenKind): Boolean = {
    kind match {
      case EOF       => ""        startsWith prefix
      case COLON     => ":"       startsWith prefix
      case SEMICOLON => ";"       startsWith prefix
      case DOT       => "."       startsWith prefix
      case COMMA     => ","       startsWith prefix
      case EQSIGN    => "="       startsWith prefix
      case EQUALS    => "=="      startsWith prefix
      case BANG      => "!"       startsWith prefix
      case LPAREN    => "("       startsWith prefix
      case RPAREN    => ")"       startsWith prefix
      case LBRACKET  => "["       startsWith prefix
      case RBRACKET  => "]"       startsWith prefix
      case LBRACE    => "{"       startsWith prefix
      case RBRACE    => "}"       startsWith prefix
      case AND       => "&&"      startsWith prefix
      case OR        => "||"      startsWith prefix
      case LESSTHAN  => "<"       startsWith prefix
      case PLUS      => "+"       startsWith prefix
      case MINUS     => "-"       startsWith prefix
      case TIMES     => "*"       startsWith prefix
      case DIV       => "/"       startsWith prefix
      case OBJECT    => "object"  startsWith prefix
      case CLASS     => "class"   startsWith prefix
      case DEF       => "def"     startsWith prefix
      case VAR       => "var"     startsWith prefix
      case UNIT      => "unit"    startsWith prefix
      case MAIN      => "main"    startsWith prefix
      case STRING    => "string"  startsWith prefix
      case EXTENDS   => "extends" startsWith prefix
      case INT       => "int"     startsWith prefix
      case BOOLEAN   => "boolean" startsWith prefix
      case WHILE     => "while"   startsWith prefix
      case IF        => "if"      startsWith prefix
      case ELSE      => "else"    startsWith prefix
      case RETURN    => "return"  startsWith prefix
      case LENGTH    => "length"  startsWith prefix
      case TRUE      => "true"    startsWith prefix
      case FALSE     => "false"   startsWith prefix
      case THIS      => "this"    startsWith prefix
      case NEW       => "new"     startsWith prefix
      case PRINTLN   => "println" startsWith prefix

      case LINECOMMENT  => "//" startsWith prefix
      case BLOCKCOMMENT => "/*" startsWith prefix

      case IDKIND    => prefix matches "^([a-zA-Z][a-zA-Z0-9_]*)?$"
      case STRLITKIND => (prefix == "") || (prefix matches """^"[^"]*"?$""")
      case INTLITKIND => prefix matches "0|[1-9][0-9]*"

      case BAD => true
      case _ => false
    }
  }

  def isToken(word: String, kind: TokenKind): Boolean = {
    kind match {
      case EOF       => false
      case COLON     => ":"       equals word
      case SEMICOLON => ";"       equals word
      case DOT       => "."       equals word
      case COMMA     => ","       equals word
      case EQSIGN    => "="       equals word
      case EQUALS    => "=="      equals word
      case BANG      => "!"       equals word
      case LPAREN    => "("       equals word
      case RPAREN    => ")"       equals word
      case LBRACKET  => "["       equals word
      case RBRACKET  => "]"       equals word
      case LBRACE    => "{"       equals word
      case RBRACE    => "}"       equals word
      case AND       => "&&"      equals word
      case OR        => "||"      equals word
      case LESSTHAN  => "<"       equals word
      case PLUS      => "+"       equals word
      case MINUS     => "-"       equals word
      case TIMES     => "*"       equals word
      case DIV       => "/"       equals word
      case OBJECT    => "object"  equals word
      case CLASS     => "class"   equals word
      case DEF       => "def"     equals word
      case VAR       => "var"     equals word
      case UNIT      => "unit"    equals word
      case MAIN      => "main"    equals word
      case STRING    => "string"  equals word
      case EXTENDS   => "extends" equals word
      case INT       => "int"     equals word
      case BOOLEAN   => "boolean" equals word
      case WHILE     => "while"   equals word
      case IF        => "if"      equals word
      case ELSE      => "else"    equals word
      case RETURN    => "return"  equals word
      case LENGTH    => "length"  equals word
      case TRUE      => "true"    equals word
      case FALSE     => "false"   equals word
      case THIS      => "this"    equals word
      case NEW       => "new"     equals word
      case PRINTLN   => "println" equals word

      case LINECOMMENT  => "//" equals word
      case BLOCKCOMMENT => "/*" equals word

      case IDKIND    => word matches "^[a-zA-Z][a-zA-Z0-9_]*$"
      case STRLITKIND => word matches """^"[^"]*"$"""
      case INTLITKIND => word matches "^0|[1-9][0-9]*$"

      case BAD => true
      case _ => false
    }
  }
}
