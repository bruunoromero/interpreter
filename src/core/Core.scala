package core

import interpreter.Scope
import parser.Ast.Atom

import scala.collection.mutable.Map
/**
  * Created by bruno on 30/11/16.
  */
object Core {
  private def oper(nums: List[Any])(fn: (Double, Double) => Double): Any = nums.reduce { (a: Any, b: Any) =>
    if(a.isInstanceOf[Double] && b.isInstanceOf[Double])
      fn(a.asInstanceOf[Double], b.asInstanceOf[Double])
    else
      throw new Exception(s"Expecting type Number, but got type ${a.getClass} and ${b.getClass}")
  }

  private def print = { (scope: Scope, params: List[Any]) => params.foreach(println) }
  private def add = { (scope: Scope, params: List[Any]) => oper(params) { (a: Double, b: Double) => a + b } }
  private def subs = { (scope: Scope, params: List[Any]) => oper(params) { (a: Double, b: Double) => a - b } }
  private def div = { (scope: Scope, params: List[Any]) => oper(params) { (a: Double, b: Double) => a / b } }
  private def mult = { (scope: Scope, params: List[Any]) => oper(params) { (a: Double, b: Double) => a * b } }
  private def define = { (scope: Scope, params: List[Any]) =>
    if(params.length != 2) throw new Exception(s"Wrong number of arguments to function def, expecting 2 arguments but got ${params.length}")
    params.head match {
      case Atom(name) => scope.set(name, params(1))
      case _ => throw new Exception(s"Expecting an atom a identifier as first argument, but got ${params.head.getClass}")
    }
    params(1)
  }

  val builtins = Map(
    "println" -> print,
    "+" -> add,
    "-" -> subs,
    "/" -> div,
    "*" -> mult,
    "def" -> define
  )
}
