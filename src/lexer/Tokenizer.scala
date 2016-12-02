package lexer

/**
  * Created by bruno on 28/11/16.
  */
object Tokenizer {
  def tokenize(content: String) = {
    val cList = content.toList
    tokenizer(cList, List[Token.Token]())
  }

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

  def readString(chars: List[Char], acc: String): (Token.Token, List[Char]) = {
    if(chars.isEmpty) throw new Exception(s"Could not parse $acc")
    else if(chars.head == '"' && acc.last != '\\')
      (Token.StringLiteral(acc), chars.tail)
    else
      readString(chars.tail, acc + chars.head.toString)
  }

  def readNumber(chars: List[Char], acc: String): (Token.Token, List[Char]) = {
    if(chars.head.isLetter || chars.head == '"') throw new Exception(s"Could not parse $acc")
    else if(chars.head.isDigit || (chars.head == '.' && !acc.contains('.')))
      readNumber(chars.tail, acc + chars.head.toString)
    else
      (Token.Number(acc), chars)
  }

  def readIdentifier(chars: List[Char], acc: String): (Token.Token, List[Char]) = chars.head match {
    case '(' | ')' | '[' | ']' | '{' | '}' | '\u0020' | '\u0009' | '\u000D' | '\u000A' =>
      (Token.Identifier(acc), chars)
    case c =>
      readIdentifier(chars.tail, acc + c.toString)
  }

  def readComment(chars: List[Char]): (Token.Token, List[Char])  = chars.head match {
    case '\n' => (Token.Nothing(), chars.tail)
    case _ => readComment(chars.tail)
  }

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
        case '\u0020' | '\u0009' | '\u000D' | '\u000A' => nextToken(chars.tail)
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' => readNumber(chars.tail, chars.head.toString)
        case _ => readIdentifier(chars.tail, chars.head.toString)
      }
  }
}
