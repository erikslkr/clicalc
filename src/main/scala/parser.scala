package de.erikslkr

import TokenType.*
import error.SyntaxError

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

  @throws[SyntaxError]
  def parse(): Expression = {
    additiveExpression()
  }

  @throws[SyntaxError]
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

  @throws[SyntaxError]
  private def multiplicativeExpression(): Expression = {
    var expression = atomicExpression()
    while (currentToken.tokenType == Multiply || currentToken.tokenType == Divide || currentToken.tokenType == LParen) {
      val operator = if (currentToken.tokenType == LParen) {
        Multiply
      } else {
        currentToken.tokenType
      }
      advance()
      val right = atomicExpression()
      expression = BinaryExpression(expression, operator, right)
    }
    expression
  }

  @throws[SyntaxError]
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
          throw new SyntaxError("Unmatched opening parenthesis")
        }
        advance()
        expression
      case RParen =>
        throw new SyntaxError("Unmatched closing parenthesis")
      case _ => throw new SyntaxError(s"Unexpected token: ${currentToken.tokenType}")
    }
  }
}
