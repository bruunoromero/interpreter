package interpreter

import scala.collection.mutable.Map

/**
  * Created by bruno on 01/12/16.
  */

case class Scope(scopes: List[Map[String, (Scope, List[Any]) => Any]]) {
  def extend = {
    val newScope = Map[String, (Scope, List[Any]) => Any]()
    Scope(newScope :: scopes)
  }

  def get(atom: String): (Scope, List[Any]) => Any = {
    get(atom, scopes)
  }

  private def get(atom: String, scopes: List[Map[String, (Scope, List[Any]) => Any]]): (Scope, List[Any]) => Any = {
    if(scopes.isEmpty) throw new Exception(s"Unexpected identifier $atom")
    scopes.head.get(atom) match {
      case Some(thing) => thing
      case None => get(atom, scopes.tail)
    }
  }

  def set(atom: String, value: Any) = {
    val scope = scopes.head
    def const = { (_: Scope, _: List[Any]) => value }
    scope += atom -> const
  }
}
