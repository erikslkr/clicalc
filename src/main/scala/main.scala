package de.erikslkr

@main
def main(args: String*): Unit = {
  try {
    val calculation = args.mkString(" ")
    val tokenizer = Tokenizer(calculation)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parse()
    val result = evaluate(ast)
    println(result)
  } catch {
    case e: Exception => println(e.getMessage)
  }
}
