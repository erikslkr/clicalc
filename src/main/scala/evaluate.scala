package de.erikslkr

import TokenType._

def evaluate(expression: Expression): BigDecimal = {
  expression match {
    case AtomicExpression(value) => value
    case BinaryExpression(left, op, right) =>
      val leftEval = evaluate(left)
      val rightEval = evaluate(right)
      op match {
        case Plus => leftEval + rightEval
        case Minus => leftEval - rightEval
        case Multiply => leftEval * rightEval
        case Divide =>
          if (rightEval == 0.0) {
            throw RuntimeException("Error: Division by zero")
          }
          leftEval / rightEval
      }
  }
}
