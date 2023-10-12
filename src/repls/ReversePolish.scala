package repls

import scala.collection.mutable.Stack

object ReversePolish {

  def isNumber(s: String): Boolean = {
    if (s.isEmpty) return false
    for (c <- s.toCharArray) {
      if (!c.isDigit) return false
    }
    return true
  }


  // converts reverse polish string to expression tree
  //
  // example :
  //   "1 3 5 + * 2 -"
  //   => Operator(Operator(Constant(1), "*", Operator(repls.Const(3), "+", repls.Const(5)), "-", repls.Const(2)))
  def reversePolishToExpression(expression: String): Expression = {
    val s: Stack[Expression] = new Stack()
    for (el <- expression.split(" ")) {
      if (isOperator(el)) {
        val rhs = s.pop
        val lhs = s.pop
        val res = Operator(lhs, el, rhs)
        s.push(res)
      } else if (isNumber(el)) s.push(Constant(el.toInt))
      else throw new Error("Unknown expression element " + el)
    }
    s.top
  }


  private def isOperator(s: String) = s == "+" || s == "*" || s == "-"
}
