package de.erikslkr
package error

class SyntaxError(message: String = "") extends Error(s"Syntax Error: $message")
