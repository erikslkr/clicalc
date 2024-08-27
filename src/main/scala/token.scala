package de.erikslkr

import TokenType._

object TokenType extends Enumeration {
  type TokenType = Value
  val Number, Plus, Minus, Multiply, Divide, LParen, RParen, EOF = Value
}

case class Token(tokenType: TokenType, value: Option[String] = None)

class Tokenizer(input: String) {
  private var index = 0

  private def currentChar: Char = {
    if (index >= input.length) {
      '\u0000'
    } else {
      input(index)
    }
  }

  private def advance(): Unit = {
    index += 1
  }

  private def number(): String = {
    val start = index
    while (currentChar.isDigit) {
      advance()
    }
    input.substring(start, index)
  }

  def tokenize(): List[Token] = {
    var tokens = List[Token]()
    while (index < input.length) {
      currentChar match {
        case ' ' | '\t' | '\n' | '\r' => advance()
        case '+' => tokens :+= Token(Plus); advance()
        case '-' => tokens :+= Token(Minus); advance()
        case '*' => tokens :+= Token(Multiply); advance()
        case '/' => tokens :+= Token(Divide); advance()
        case '(' => tokens :+= Token(LParen); advance()
        case ')' => tokens :+= Token(RParen); advance()
        case char if char.isDigit => tokens :+= Token(Number, Some(number()))
        case _ => throw RuntimeException(s"Illegal character: $currentChar")
      }
    }
    tokens :+= Token(EOF)
    tokens
  }
}
