package repls

import scala.collection.mutable.Stack

object ReversePolish {

  def isNumber(s: String): Boolean = {
    var sCopy = s

    if(s.contains("-"))
    {
      sCopy = s.dropWhile(_ == '-')
    }

    if (sCopy.isEmpty) return false
    for (c <- sCopy.toCharArray) {
      if (!c.isDigit) return false
    }
    return true
  }

  def isVar(s: String): Boolean = {
    if(s.isEmpty) return false

    val isFirstCharLetter = s.headOption.exists(_.isLetter)
    if(!isFirstCharLetter)
    {
      return false
    }
    return true
  }


  // converts reverse polish string to expression tree
  //
  // example :
  //   "1 3 5 + * 2 -"
  //   => Operator(Operator(Constant(1), "*", Operator(repls.Const(3), "+", repls.Const(5)), "-", repls.Const(2)))
  def reversePolishToExpression(expression: String, varMap: Map[String, Int]): Expression = {
    val s: Stack[Expression] = new Stack()
    /*for (el <- expression.split(" ")) {
      if (isOperator(el)) {
        val rhs = s.pop
        val lhs = s.pop
        val res = Operator(lhs, el, rhs)
        s.push(res)
      } else if (isNumber(el)) s.push(Constant(el.toInt))
      else if (isVar(el)) {
        s.push(Var(el))

      }
      else if(el == "=") el continue
      else throw new Error("Unknown expression element " + el)
    }*/
    val elements = expression.split(" ")
    var i = 0
    while (i < elements.length)
    {
      if (isOperator(elements(i))) {
        val rhs = s.pop
        val lhs = s.pop
        val res = Operator(lhs, elements(i), rhs)
        s.push(res)
      }
      else if (isNumber(elements(i))) s.push(Constant(elements(i).toInt))
      else if (isVar(elements(i)))
      {
        val variableVal = varMap(elements(i))
        s.push(Constant(variableVal))
      }
      else throw new Error("Unknown expression element " + elements(i))
      i += 1
    }
    s.top
  }


  private def isOperator(s: String) = s == "+" || s == "*" || s == "-"
}
