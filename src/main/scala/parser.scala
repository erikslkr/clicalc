package de.erikslkr

import TokenType._

sealed trait Expression

case class AtomicExpression(value: BigDecimal) extends Expression
case class BinaryExpression(left: Expression, op: TokenType, right: Expression) extends Expression

class Parser(tokens: List[Token]) {
  private var index = 0

  private def currentToken: Token = {
    tokens(index)
  }

  private def advance(): Unit = {
    index += 1
  }

  def parse(): Expression = {
    additiveExpression()
  }

  private def additiveExpression(): Expression = {
    var expression = multiplicativeExpression()
    while (currentToken.tokenType == Plus || currentToken.tokenType == Minus) {
      val operator = currentToken.tokenType
      advance()
      val right = multiplicativeExpression()
      expression = BinaryExpression(expression, operator, right)
    }
    expression
  }

  private def multiplicativeExpression(): Expression = {
    var expression = atomicExpression()
    while (currentToken.tokenType == Multiply || currentToken.tokenType == Divide) {
      val operator = currentToken.tokenType
      advance()
      val right = atomicExpression()
      expression = BinaryExpression(expression, operator, right)
    }
    expression
  }

  private def atomicExpression(): Expression = {
    currentToken.tokenType match {
      case Number =>
        val value = BigDecimal(currentToken.value.get)
        advance()
        AtomicExpression(value)
      case LParen =>
        advance()
        val expression = additiveExpression()
        if (currentToken.tokenType != RParen) {
          throw RuntimeException("Unmatched opening parenthesis")
        }
        advance()
        expression
      case RParen =>
        throw RuntimeException("Unmatched closing parenthesis")
      case _ => throw RuntimeException(s"Unexpected token: ${currentToken.tokenType}")
    }
  }
}
