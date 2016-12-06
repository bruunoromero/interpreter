package parser

import interpreter.Environment
import core.Core.NativeFunction

import scala.runtime.BoxedUnit

/**
  * Created by bruno on 29/11/16.
  */
object Ast {

  sealed trait Node {
    def eval(env: Environment): Any
  }

  class Value(value: Any) extends Node {
    override def eval(env: Environment) = value
  }

  case class Null() extends Value(null) with Node {
    override def eval(env: Environment): Any = null
  }

  case class Program(body: List[Ast.Node]) extends Node {
    override def eval(env: Environment) = body.map(el => el.eval(env))
  }

  case class Number(value: Double) extends Value(value) with Node

  case class Bool(value: Boolean) extends Value(value) with Node

  case class StringLiteral(value: String) extends Value(value) with Node

  case class Atom(name: String) extends Node {
    override def eval(env: Environment) = env.get(name)
  }

  case class Expr(name: String, params: List[Ast.Node]) extends Node {
    override def eval(env: Environment) = {
      def thing = env.get(name)
      thing match {
        case NativeFunction(arity, _) =>
          if(arity.isInstanceOf[String] && arity.asInstanceOf[String] == "n" ||
            (arity.isInstanceOf[Int] && arity.asInstanceOf[Int] == params.length)) {
            val values = this.params.map(el => el.eval(env))
            thing.asInstanceOf[NativeFunction].eval(values, env)
          }
          else
            throw new Exception(s"Unexpected number of arguments for function $name")
        case FunctionLiteral(arity, params, body) =>
          if(this.params.length == params.length)
            thing.asInstanceOf[FunctionLiteral].eval(this.params, env)
          else throw new Exception(s"Unexpected number of arguments for function $name")
        case _ => throw new Exception(s"Unexpected identifier $name")
      }
    }
  }

  case class ListLiteral(value: List[Ast.Node]) extends Value(value) with Node

  case class Assign(name: String, value: Ast.Node) extends Node {
    override def eval(env: Environment): Unit = {
      value match {
        case Number(content) => env.set(name, content)
        case StringLiteral(content) => env.set(name, content)
        case Bool(content) => env.set(name, content)
        case ListLiteral(content) => env.set(name, content)
        case Atom(content) => env.set(name, value.eval(env))
        case FuncDefinition(_, _) => env.set(name, value.eval(env))
        case t => throw new Exception(s"Cannot assign values of type $t")
      }
    }
  }

  case class FunctionLiteral(arity: Integer, params: List[Atom], body: Node) {
    def eval(params: List[Ast.Node], env: Environment) = {
      val scope = env.extend
      val values = params.map(el => el.eval(scope))
      for(i <- this.params.indices) {
        scope.set(this.params(i).name, values(i))
      }
      body.eval(scope)
    }
  }

  case class FuncDefinition(params: ListLiteral, body: Ast.Node) extends Node {
    override def eval(env: Environment) = {
      FunctionLiteral(params.value.length, params.value.map(el => el.asInstanceOf[Atom]), body)
    }
  }

  case class IfNode(cond: Node, ifTrue: Node, ifFalse: Node) extends Node {
    override def eval(env: Environment) = {
      val res = cond.eval(env)
      if(res.isInstanceOf[Boolean])
        if(res == true)
          ifTrue.eval(env)
        else
          ifFalse.eval(env)
      else
        throw new Exception(s"Expecting a Bool but got ${res.getClass}")
    }
  }

}
