import interpreter.Interpreter
import lexer.Tokenizer
import parser.Parser

import scala.io.Source

object Main extends App {
  val content = Source.fromFile("/Users/bruno/lang.glang").mkString
  val tokens = Tokenizer.tokenize(content)
  val ast = Parser.parse(tokens)
  Interpreter.interpret(ast)
}
