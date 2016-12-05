package core

import core.Core.NativeFunction
import interpreter.Environment

/**
  * Created by bruno on 03/12/16.
  */
protected object StringLiteral extends Type {
  def concat = NativeFunction("n", (env: Environment, params: List[Any]) => apply[String](params, "String") { (a: String, b: String) => a + b })
}
