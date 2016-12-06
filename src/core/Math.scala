package core

import core.Core.NativeFunction
import interpreter.Environment

/**
  * Created by bruno on 03/12/16.
  */
protected object Math extends Type {
  val add = NativeFunction("n", (env: Environment, params: List[Any]) => apply[Any](params, "Number") {
    (a: Any, b: Any) =>
      if(a.isInstanceOf[Long] && b.isInstanceOf[Long])
        a.asInstanceOf[Long] + b.asInstanceOf[Long]
      else if(a.isInstanceOf[Double] && b.isInstanceOf[Double])
        a.asInstanceOf[Double] + b.asInstanceOf[Double]
      else
        throw new Exception(s"Expecting types Integer and Integer or Double and Double, but got type ${a.getClass} and ${b.getClass}")
  })

  val subs = NativeFunction("n", (env: Environment, params: List[Any]) => apply[Any](params, "Number") {
    (a: Any, b: Any) =>
      if(a.isInstanceOf[Long] && b.isInstanceOf[Long])
        a.asInstanceOf[Long] - b.asInstanceOf[Long]
      else if(a.isInstanceOf[Double] && b.isInstanceOf[Double])
        a.asInstanceOf[Double] - b.asInstanceOf[Double]
      else
        throw new Exception(s"Expecting types Integer and Integer or Double and Double, but got type ${a.getClass} and ${b.getClass}")
  })

  val div = NativeFunction("n", (env: Environment, params: List[Any]) => apply[Any](params, "Number") {
    (a: Any, b: Any) =>
      if(a.isInstanceOf[Long] && b.isInstanceOf[Long])
        a.asInstanceOf[Long] / b.asInstanceOf[Long]
      else if(a.isInstanceOf[Double] && b.isInstanceOf[Double])
        a.asInstanceOf[Double] / b.asInstanceOf[Double]
      else
        throw new Exception(s"Expecting types Integer and Integer or Double and Double, but got type ${a.getClass} and ${b.getClass}")
  })

  val mult = NativeFunction("n", (env: Environment, params: List[Any]) => apply[Any](params, "Number") {
    (a: Any, b: Any) =>
      if(a.isInstanceOf[Long] && b.isInstanceOf[Long])
        a.asInstanceOf[Long] * b.asInstanceOf[Long]
      else if(a.isInstanceOf[Double] && b.isInstanceOf[Double])
        a.asInstanceOf[Double] * b.asInstanceOf[Double]
      else
        throw new Exception(s"Expecting types Integer and Integer or Double and Double, but got type ${a.getClass} and ${b.getClass}")
  })
}
