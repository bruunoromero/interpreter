package core

import interpreter.Environment
import interpreter.Environment.Value
import parser.Ast._

import scala.collection.mutable.Map
/**
  * Created by bruno on 30/11/16.
  */
object Core {
  case class NativeFunction(arity: Any, func: (Environment, List[Any]) => Any) {
    def eval(params: List[Any], env: Environment) = {
      func(env, params)
    }
  }

  private val `do` = NativeFunction("n", (env: Environment, params: List[Any]) => {
    val res = params.map { el =>
      if(el.isInstanceOf[Node])
        el.asInstanceOf[Node].eval(env)
      else
        el
    }

    res.last
  })

  val builtins = Map[String, Value](
    "println" -> IO.printLine,
    "read-line" -> IO.readLine,
    "do" -> `do`,
    "+" -> Math.add,
//    "-" -> Math.subs,
//    "/" -> Math.div,
//    "*" -> Math.mult,
    "=" -> Math.eq
//    "<>" -> StringLiteral.concat
  )
}
