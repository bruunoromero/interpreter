package interpreter

import core.Core
import parser.Ast
import parser.Ast._

/**
  * Created by bruno on 30/11/16.
  */
object Interpreter {
  private val rootScope = Environment(List(Core.builtins)).extend

  def interpret(ast: Ast.Program) = {
    ast.eval(rootScope)
  }
}
