package core

import core.Core.NativeFunction
import interpreter.Environment

/**
  * Created by bruno on 03/12/16.
  */
protected object Math extends Type {
  val add = NativeFunction("n", (env: Environment, params: List[Any]) => apply[Double](params, "Number") { (a: Double, b: Double) => a + b })
  val subs = NativeFunction("n", (env: Environment, params: List[Any]) => apply[Double](params, "Number") { (a: Double, b: Double) => a - b })
  val div = NativeFunction("n", (env: Environment, params: List[Any]) => apply[Double](params, "Number") { (a: Double, b: Double) => a / b })
  val mult = NativeFunction("n", (env: Environment, params: List[Any]) => apply[Double](params, "Number") { (a: Double, b: Double) => a * b })
}
