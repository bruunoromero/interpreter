package core

import core.Core.NativeFunction
import interpreter.Environment

/**
  * Created by bruno on 03/12/16.
  */
protected trait Type {
  def stringfy = NativeFunction("n", (env: Environment, params: List[Any]) => {
    if(params.length != 1) throw new Exception(s"Wrong number of arguments, was passed ${params.length} arguments but is expecting 1")
    params.head.toString
  })

  val eq = NativeFunction(2, (env: Environment, params: List[Any]) => {
    if(params.head.getClass == params(1).getClass)
      params.head == params(1)
    else
      throw new Exception(s"function = can only compare values of same type")
  })

  def apply[A](nums: List[Any], expct: String)(fn: (A, A) => A): Any = nums.reduce { (a: Any, b: Any) =>
    if(a.isInstanceOf[A] && b.isInstanceOf[A])
      fn(a.asInstanceOf[A], b.asInstanceOf[A])
    else
      throw new Exception(s"Expecting type $expct, but got type ${a.getClass} and ${b.getClass}")
  }
}
