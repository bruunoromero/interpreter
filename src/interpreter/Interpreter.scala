package interpreter

import core.Core
import parser.Ast
import parser.Ast._

/**
  * Created by bruno on 30/11/16.
  */
object Interpreter {
  private val rootScope = Scope(List(Core.builtins)).extend

  def interpret(ast: Ast.Program) = {
    ast.body.map(el => eval(el, rootScope))
  }

  private def eval(node: Ast.Node, scope: Scope): Any = {
    node match {
      case Expr(name, params) =>
        val values = params.map(el => eval(el, scope))
        exec(name, values, scope)
      case Assign(name, content, params) =>
        val values = params.map(el => eval(el, scope))
        exec(name, content :: values, scope)
      case StringLiteral(value) => value
      case Number(value) => value
      case Atom(name) => exec(name, List(), scope)
    }
  }

  private def exec(name: String, params: List[Any], scope: Scope) = {
    def thing = scope.get(name)
    thing(scope, params)
  }
}
