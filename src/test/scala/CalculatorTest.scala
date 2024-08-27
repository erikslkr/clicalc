package de.erikslkr

import org.scalatest.funsuite.AnyFunSuite

class CalculatorTest extends AnyFunSuite {

  test("Addition of two integers") {
    assert(calculate("2 + 3") == 5)
  }

  test("Addition of two floating point numbers") {
    assert(calculate("2.5 + 3.2") == 5.7)
  }

  test("Subtraction of two integers") {
    assert(calculate("5 - 2") == 3)
  }

  test("Subtraction of two floating point numbers") {
    assert(calculate("5.5 - 2.2") == 3.3)
  }

  test("Multiplication of two integers") {
    assert(calculate("3 * 4") == 12)
  }

  test("Multiplication of two floating point numbers") {
    assert(calculate("2.5 * 2") == 5.0)
  }

  test("Division of two integers") {
    assert(calculate("10 / 2") == 5)
  }

  test("Division of two floating point numbers") {
    assert(calculate("7.5 / 2.5") == 3.0)
  }

  test("Division by zero should throw an exception") {
    assertThrows[RuntimeException] {
      calculate("10 / 0")
    }
  }

  test("Complex expression with mixed operations") {
    assert(calculate("3 + 5 * 2 - 4 / 2") == 11)
  }

  test("Complex expression with floating point numbers") {
    assert(calculate("3.5 + 2.5 * 2 - 1.0 / 0.5") == 6.5)
  }
}
