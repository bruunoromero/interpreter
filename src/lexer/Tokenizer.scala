package lexer

import scala.annotation.tailrec

/**
  * Created by bruno on 28/11/16.
  */
object Tokenizer {
  def tokenize(content: String) = {
    val cList = content.toList
    tokenizer(cList, List[Token.Token]())
  }

  @tailrec
  private def tokenizer(chars: List[Char], tokens: List[Token.Token]): List[Token.Token] = {
    if(chars.isEmpty) tokens.reverse
    else {
      val res = nextToken(chars)
      res._1 match  {
        case Token.Nothing() => tokenizer(res._2, tokens)
        case _ => tokenizer(res._2, res._1 :: tokens)
      }
    }
  }

  @tailrec
  private def readString(chars: List[Char], acc: String): (Token.Token, List[Char]) = {
    if(chars.isEmpty) throw new Exception(s"Could not parse $acc")
    else if(chars.head == '"' && acc.last != '\\')
      (Token.StringLiteral(acc), chars.tail)
    else
      readString(chars.tail, acc + chars.head.toString)
  }

  @tailrec
  private def readNumber(chars: List[Char], acc: String): (Token.Token, List[Char]) = {
    if(chars.head.isLetter || chars.head == '"') throw new Exception(s"Could not parse $acc")
    else if(chars.head.isDigit || (chars.head == '.' && !acc.contains('.')))
      readNumber(chars.tail, acc + chars.head.toString)
    else
      (Token.Number(acc), chars)
  }

  @tailrec
  private def readIdentifier(chars: List[Char], acc: String): (Token.Token, List[Char]) = chars.head match {
    case '(' | ')' | '[' | ']' | '{' | '}' | '\u0020' | '\u0009' | '\u000D' | '\u000A' =>
      acc match {
        case "true" | "false" => (Token.Bool(acc), chars)
        case _ => (Token.Identifier(acc), chars)
      }
    case c =>
      readIdentifier(chars.tail, acc + c.toString)
  }

  @tailrec
  private def readComment(chars: List[Char]): (Token.Token, List[Char])  = chars.head match {
    case '\n' => (Token.Nothing(), chars.tail)
    case _ => readComment(chars.tail)
  }

  private def chooseNext(chars: List[Char], acc: String) = chars.head match {
    case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' => readNumber(chars.tail, acc + chars.head.toString)
    case '(' | ')' | '[' | ']' | '{' | '}' | '\u0020' | '\u0009' | '\u000D' | '\u000A' => (Token.Identifier(acc), chars)
    case _ => readIdentifier(chars.tail, acc + chars.head.toString)
  }

  @tailrec
  private def nextToken(chars: List[Char]): (Token.Token, List[Char]) = {
    if(chars.isEmpty) (Token.Nothing(), List[Char]())
    else
      chars.head match {
        case '(' => (Token.LeftParen(), chars.tail)
        case ')' => (Token.RightParen(), chars.tail)
        case '[' => (Token.LeftBrace(), chars.tail)
        case ']' => (Token.RightBrace(), chars.tail)
        case '{' => (Token.LeftCurly(), chars.tail)
        case '}' => (Token.RightCurly(), chars.tail)
        case '"' => readString(chars.tail, "")
        case ';' => readComment(chars.tail)
        case '+' | '-' => chooseNext(chars.tail, chars.head.toString)
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' => readNumber(chars.tail, chars.head.toString)
        case '\u0020' | '\u0009' | '\u000D' | '\u000A' => nextToken(chars.tail)
        case _ => readIdentifier(chars.tail, chars.head.toString)
      }
  }
}
