package core

import java.util.Scanner

import interpreter.Environment
import core.Core.NativeFunction

/**
  * Created by bruno on 03/12/16.
  */
protected object IO {
  val printLine = NativeFunction("n", (env: Environment, params: List[Any]) => params.foreach(println))
  val readLine = NativeFunction(0, (env: Environment, params: List[Any]) => {
    val scanner = new Scanner(System.in)
    scanner.nextLine()
  })

}
