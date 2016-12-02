package parser

/**
  * Created by bruno on 29/11/16.
  */
object Ast {

  sealed trait Node

  case class Program(body: List[Ast.Node]) extends Node

  case class Number(value: Double) extends Node

  case class StringLiteral(value: String) extends Node

  case class Atom(name: String) extends Node

  case class Expr(name: String, params: List[Ast.Node]) extends Node

  case class Assign(name: String, atom: Atom, params: List[Ast.Node]) extends Node

}
