package parser

import com.sun.tools.javac.parser.Tokens
import lexer.Token
import parser.Ast._

import scala.annotation.tailrec

/**
  * Created by bruno on 29/11/16.
  */
object Parser {
  def parse(tokens: List[Token.Token]): Program = parse(tokens, Program(List[Node]()))

  @tailrec
  private def parse(tokens: List[Token.Token], ast: Program): Ast.Program = {
    if(tokens.isEmpty) Ast.Program(ast.body.reverse)
    else {
      val res = readToken(tokens)
      parse(res._1, Ast.Program(res._2 :: ast.body))
    }
  }

  @tailrec
  private def readExprCall(tokens: List[Token.Token], node: Expr): (List[Token.Token], Node) = tokens.head match {
    case Token.RightParen() =>
      (tokens.tail, Expr(node.name, node.params.reverse))
    case _ =>
      val res = readToken(tokens)
      val params = res._2 :: node.params
      readExprCall(res._1, Ast.Expr(node.name, params))
  }

  def readDefinitionCall(name: String, tokens: List[Token.Token]): (List[Token.Token], Node) =  {
    val res = readToken(tokens)
    if(res._1.head.isInstanceOf[Token.RightParen])
      (res._1.tail, Assign(name, res._2))
    else
      throw new Exception(s"Could not parse identifier ${res._1.head.getClass}")
  }

  private def readFunctionDefinition(tokens: List[Token.Token]) = {
    val res = readList(tokens, ListLiteral(List[Node]()))
    val list = res._2.asInstanceOf[ListLiteral]
    val res2 = readToken(res._1)
    if(res2._1.head.isInstanceOf[Token.RightParen]) {
      (res2._1.tail, FuncDefinition(list, res2._2))
    } else {
      throw new Exception(s"Could not parse identifier ${res._1.head.getClass}")
    }
  }

  private def readCallType(tokens: List[Token.Token]) = tokens.head match {
    case Token.Identifier(name) =>
      name match {
        case "def" =>
          tokens(1) match {
            case Token.Identifier(content) => readDefinitionCall(content, tokens.tail.tail)
            case t => throw new Exception(s"Unexpected token of type $t, expecting an identifier")
          }
        case "fun" =>
          tokens(1) match {
            case Token.LeftBrace() =>
              readFunctionDefinition(tokens.tail.tail)
            case t => throw new Exception(s"Unexpected token of type $t, expecting a [")
          }
        case "if" =>
          val cond = readToken(tokens.tail)
          cond._2 match {
            case Ast.Bool(_) | Ast.Expr(_, _) | Ast.Atom(_) =>
              val ifTrue = readToken(cond._1)
              val ifFalse = readToken(ifTrue._1)
              (ifFalse._1.tail, IfNode(cond._2, ifTrue._2, ifFalse._2))
            case _ => throw new Exception(s"Unexpected token of type ${cond._2.getClass}")
          }
        case _ => readExprCall(tokens.tail, Ast.Expr(name, List[Ast.Node]()))
      }
    case t => throw new Exception(s"Unexpected token of type $t, expecting type Identifier")
  }

  @tailrec
  def readList(tokens: List[Token.Token], node: ListLiteral): (List[Token.Token], Node) = tokens.head match {
    case Token.RightBrace() =>
      (tokens.tail, ListLiteral(node.value.reverse))
    case _ =>
      val res = readToken(tokens)
      val params = res._2 :: node.value
      readList(res._1, ListLiteral(params))
  }

  private def readToken(tokens: List[Token.Token]): (List[Token.Token], Node) = {
    if(tokens.isEmpty) throw new Exception("Something is wrong with your code")
    tokens.head match {
      case Token.Number(value) => (tokens.tail, Number(value.toDouble))
      case Token.Bool(value) => (tokens.tail, Bool(value.toBoolean))
      case Token.StringLiteral(value) => (tokens.tail, StringLiteral(value))
      case Token.LeftParen() => readCallType(tokens.tail)
      case Token.LeftBrace() => readList(tokens.tail, ListLiteral(List[Node]()))
      case Token.Identifier(name) =>
        name match {
          case "null" => (tokens.tail, Null())
          case _ => (tokens.tail, Atom(name))
        }
      case Token.RightBrace() => throw new Exception("Unexpected token ']'")
      case Token.RightParen() => throw new Exception("Unexpected token ')'")
      case _ => throw new Exception(s"Unexpected token '${tokens.head}'")
    }
  }
}
