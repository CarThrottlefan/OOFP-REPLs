package repls

import scala.collection.mutable.Stack
import scala.util.matching.Regex

object ReversePolish {

  def isNumber(s: String): Boolean =
  {
    var sCopy = s

    if(s.contains("-"))
    {
      sCopy = s.dropWhile(_ == '-')
    }

    if (sCopy.isEmpty)
      return false

    for (c <- sCopy.toCharArray)
    {
      if (!c.isDigit)
        return false
    }
    return true
  }

  def isVar(s: String): Boolean = {
    if(s.isEmpty)
      return false

    val isFirstCharLetter = s.headOption.exists(_.isLetter)
    if(!isFirstCharLetter)
    {
      return false
    }
    return true
  }

  def reversePolishToExpression(expression: String, varMap: Map[String, Int]): Expression =
  {
    val s: Stack[Expression] = new Stack()
    val elements = expression.split(" ")
    var i = 0
    while (i < elements.length)
    {
      if (isOperator(elements(i)))
      {
        val rhs = s.pop
        val lhs = s.pop
        val res = Operator(lhs, elements(i), rhs)
        s.push(res)
      }
      else if (isNumber(elements(i)))
      {
        val constantValue = Constant(elements(i).toInt)
        s.push(constantValue)
      }
      else if (isVar(elements(i)))
      {
        if(varMap.contains(elements(i)))
        {
          val variableVal = varMap(elements(i))
          s.push(Constant(variableVal))
        }
        else
        {
          s.push(Var(elements(i)))
        }
      }
      else throw new Error("Unknown expression element " + elements(i))
      i += 1
    }
    s.top
  }

  private def isOperator(s: String) = s == "+" || s == "*" || s == "-"
}
