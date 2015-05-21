package ru.naumen.testtask.calculator

import java.util.{NoSuchElementException, StringTokenizer}
import scala.collection.mutable.Stack;

/**
 * This class contains methods which allow you to calculate mathematical expressions.
 * It implements Dijkstra's shunting-yard algorithm to calculate expressions.
 * It also contains methods to get reversed polish notation and calculate it.
 * @author Andrei Selkin
 * @version 0.0.3
 */
class RPNCalculator extends Calculator {

  // Precedences of operations according to Dijkstra's shunting-yard algorithm
  val precedences = Map("+" -> 1, "-" -> 1, "*" -> 2, 
    "/" -> 2, "^" -> 3, "!" -> 4, "s" -> 5, "c" -> 5)

  /**
   * Computes an expression.
   * @param input - expression.
   * @return - result of computation.
   */
  def compute(input : String) : scala.math.BigDecimal = {
    val rpn = transformToPostfixNotation(input)
    calculatePostfixExpression(rpn)
  }

  /**
   * Transforms an infix expression to a postfix expression.
   * @param inputLine - expression in infix form.
   * @return - expression in postfix form (reversed polish notation).
   */
  def transformToPostfixNotation(inputLine: String): String = {
    val outputLine = new StringBuilder();
    val stackOperations = new Stack[String]

    // make some preparations
    var expression = inputLine.replaceAll("\\s+", "").replace("(-", "(0-")
      .replace("sin", "s").replace("cos", "c");
    if (inputLine.charAt(0) == '-') {
      expression = "0" + expression;
    }

    val tokenizer = new StringTokenizer(
      expression, precedences.keys.mkString("") + "()", true);
    // loop for handling each token - shunting-yard algorithm
    while (tokenizer.hasMoreTokens()) {
      val token = tokenizer.nextToken()
      if (isOpeningBracket(token)) {
        stackOperations.push(token)
      }
      else if (isClosingBracket(token)) {
        try {
          while (!stackOperations.isEmpty
            && !isOpeningBracket(stackOperations.top)) {
            outputLine.append(stackOperations.pop())
            outputLine.append(" ")
          }
          stackOperations.pop()
        } catch {
          // Stack had ended before open bracket was met
          case _: NoSuchElementException =>
            throw new NoSuchElementException("Opening bracket was missed!")
        }
      }
      else if (isNumber(token)) {
        outputLine.append(token)
        outputLine.append(" ")
      }
      else if (isOperator(token)) {
        while (!stackOperations.isEmpty
          && isOperator(stackOperations.top)
          && getPrecedence(token) <= getPrecedence(stackOperations.top)) {
          outputLine.append(stackOperations.pop())
          outputLine.append(" ")
        }
        stackOperations.push(token);
      }
      else {
        throw new IllegalArgumentException("Invalid token: " + token)
      }
    }
    while (!stackOperations.isEmpty) {
      if (isOperator(stackOperations.top)) {
        //There should have left only operators in the stack
        outputLine.append(stackOperations.pop())
        outputLine.append(" ")
      } else {
        throw new IllegalArgumentException("Closing bracket was missed!")
      }
    }
    outputLine.toString().trim()
  }

  private def isNumber(token: String): Boolean = {
    token.matches("^\\-?\\d+(\\.\\d{0,})?$")
  }

  private def isOperator(token: String): Boolean = {
    precedences.contains(token)
  }

  private def isOpeningBracket(token: String): Boolean = {
    token.equals("(")
  }

  private def isClosingBracket(token: String): Boolean = {
    token.equals(")")
  }

  private def getPrecedence (token: String): Int = {
    precedences.getOrElse(token, 0)
  }

  /**
   * Calculates expression which is written in reversed polish notation.
   * @param expression - expression in reversed polish notation.
   * @return - result of the calculation.
   */
  def calculatePostfixExpression(expression: String): BigDecimal = {
    val tokens = expression.split(" ")
    val stack = new Stack[BigDecimal]

    for (token <- tokens)
      if (!handleOperator(token, stack) && !handleNumber(token, stack))
        throw new IllegalArgumentException("Invalid token: " + token)

    stack.pop()
  }

  /**
   * If the token is an operator, pop operand(s) off the stack,
   * perform the operation and push the result back on.
   * @param token - token.
   * @param stack - working stack.
   * @return true if operator was handled successfully.
   */
  private def handleOperator(token: String, stack: Stack[BigDecimal]): Boolean = try {
    token match {
      case "+" =>
        val rightOperand = stack.pop()
        val leftOperand = stack.pop()
        stack.push(leftOperand.+(rightOperand))
        true
      case "-" =>
        val rightOperand = stack.pop()
        val leftOperand = stack.pop()
        stack.push(leftOperand.-(rightOperand))
        true
      case "*" =>
        val rightOperand = stack.pop()
        val leftOperand = stack.pop()
        stack.push(leftOperand.*(rightOperand))
        true
      case "/" =>
        val rightOperand = stack.pop()
        val leftOperand = stack.pop()
        stack.push(leftOperand./(rightOperand))
        true
      case "!" =>
        val singleOperand = stack.pop()
        stack.push(factTree(singleOperand))
        true
      case "^" =>
        val rightOperand = stack.pop()
        val leftOperand = stack.pop()
        stack.push(leftOperand.pow(rightOperand.toIntExact))
        true
      case "s" =>
        val singleOperand = stack.pop()
        stack.push(Math.sin(singleOperand.toDouble))
        true
      case "c" =>
        val singleOperand = stack.pop()
        stack.push(Math.cos(singleOperand.toDouble))
        true
      case _ => false
    }
  } catch {
    case _: NoSuchElementException =>
      throw new NoSuchElementException("Wrong expression format!")
  }

  /**
   * If token is a number, push it on the stack.
   * @param token - token.
   * @param stack - working stack.
   * @return true if number was handled successfully.
   */
  private def handleNumber(token: String, stack: Stack[BigDecimal]): Boolean = try {
    stack.push(BigDecimal(token))
    true
  } catch {
    case _: NumberFormatException => false
  }

  /**
   * Computes factorial using tree algorithm.
   * @param n - a number to compute factorial for.
   * @return factorial of n.
   */
  def factTree(n: BigDecimal): BigDecimal = {
    if (n < 0)
      return 0
    if (n == 0)
      return 1
    if (n == 1 || n == 2)
      return n
    prodTree(2, n)
  }

  /**
   * Computes root value which is use to compute factorial.
   * @param l - left node value.
   * @param r - right node value.
   * @return root node value
   */
  private def prodTree(l:BigDecimal, r:BigDecimal): BigDecimal = {
    if (l > r)
      return 1
    if (l == r)
      return l
    if (r - l == 1)
      return l.*(r)
    val m = (l + r) / 2;
    prodTree(l, m) * prodTree(m + 1, r);
  }
}
