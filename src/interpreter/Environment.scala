package interpreter

import interpreter.Environment._

import scala.annotation.tailrec
import scala.collection.mutable.Map

/**
  * Created by bruno on 01/12/16.
  */

object Environment {
  type Value = Any
  type Scope = Map[String, Value]
}

case class Environment(scopes: List[Environment.Scope]) {
  def extend = {
    val newScope = Map[String, Value]()
    Environment(newScope :: scopes)
  }

  def get(atom: String): Value = {
    get(atom, scopes)
  }

  @tailrec
  private def get(name: String, scopes: List[Environment.Scope]): Value = {
    if(scopes.isEmpty) throw new Exception(s"Could not find identifier $name")
    else
      scopes.head.get(name) match {
        case Some(thing) => thing
        case None => get(name, scopes.tail)
      }
  }

  def set(name: String, value: Value) = scopes.head.get(name) match {
    case Some(_) => throw new Exception(s"Value $name already defined in current scope")
    case None =>
      val scope = scopes.head
      scope += name -> value
  }
}
