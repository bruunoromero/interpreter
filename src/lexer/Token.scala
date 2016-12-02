package lexer

/**
  * Created by bruno on 28/11/16.
  */
object Token {

  sealed trait Token

  case class StringLiteral(content: String) extends Token

  case class Number(content: String) extends Token

  case class Identifier(content: String) extends Token

  case class LeftParen() extends Token

  case class RightParen() extends Token

  case class LeftBrace() extends Token

  case class RightBrace() extends Token

  case class LeftCurly() extends Token

  case class RightCurly() extends Token

  case class Nothing() extends Token

}



