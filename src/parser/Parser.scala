package parser

import lexer.Token
import lexer.Token.{Identifier, Token}

/**
  * Created by bruno on 29/11/16.
  */
object Parser {
  def parse(tokens: List[Token]): Ast.Program = parse(tokens, Ast.Program(List[Ast.Node]()))

  private def parse(tokens: List[Token], ast: Ast.Program): Ast.Program = {
    if(tokens.isEmpty) Ast.Program(ast.body.reverse)
    else {
      val res = readToken(tokens)
      parse(res._1, Ast.Program(res._2 :: ast.body))
    }
  }

  private def readExprCall(tokens: List[Token], node: Ast.Expr): (List[Token], Ast.Node) = {
    tokens.head match {
      case Token.RightParen() =>
        (tokens.tail, Ast.Expr(node.name, node.params.reverse))
      case _ =>
        val res = readToken(tokens)
        val params = res._2 :: node.params
        readExprCall(res._1, Ast.Expr(node.name, params))
    }
  }

  private def readDefinitionCall(tokens: List[Token], node: Ast.Assign): (List[Token], Ast.Node) = {
    tokens.head match {
      case Token.RightParen() =>
        (tokens.tail, Ast.Assign(node.name, node.atom, node.params.reverse))
      case _ =>
        val res = readToken(tokens)
        val params = res._2 :: node.params
        readDefinitionCall(res._1, Ast.Assign(node.name, node.atom, params))
    }
  }

  private def readCallType(tokens: List[Token]) = {
    tokens.head match {
      case Identifier(name) =>
        name match {
          case "def" =>
            tokens(1) match {
              case Identifier(content) => readDefinitionCall(tokens.tail.tail, Ast.Assign(name, Ast.Atom(content), List[Ast.Node]()))
              case t => throw new Exception(s"Unexpected token of type $t, expecting an identifier")
            }
          case _ => readExprCall(tokens.tail, Ast.Expr(name, List[Ast.Node]()))
        }
      case t => throw new Exception(s"Unexpected token of type $t, expecting type Identifier")
    }
  }

  private def readToken(tokens: List[Token]): (List[Token], Ast.Node) = {
      if(tokens.isEmpty) throw new Exception("Something is wrong with your code")
      tokens.head match {
        case Token.Number(value) => (tokens.tail, Ast.Number(value.toDouble))
        case Token.StringLiteral(value) => (tokens.tail, Ast.StringLiteral(value))
        case Token.LeftParen() => readCallType(tokens.tail)
        case Token.Identifier(name) => (tokens.tail, Ast.Atom(name))
        case Token.RightParen() => throw new Exception("Unexpected token ')'")
        case _ => throw new Exception(s"Unexpected token '${tokens.head}'")
    }
  }
}
