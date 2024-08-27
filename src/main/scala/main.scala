package de.erikslkr

import error.SyntaxError

@throws[SyntaxError]
@throws[ArithmeticException]
def calculate(calculation: String): BigDecimal = {
  val tokenizer = Tokenizer(calculation)
  val tokens = tokenizer.tokenize()
  val parser = Parser(tokens)
  val ast = parser.parse()
  val result = evaluate(ast)
  result
}

@main
def main(args: String*): Unit = {
  try {
    val calculation = args.mkString(" ")
    val result = calculate(calculation)
    println(result)
  } catch {
    case e: Throwable => println(e.getMessage)
  }
}
