package repls

import scala.collection.mutable.Stack
import scala.util.matching.Regex

object SetReversePolish
{

  def isVar(s: String): Boolean =
  {
    if(s.isEmpty) return false

    val isFirstCharLetter = s.headOption.exists(_.isLetter)
    if(!isFirstCharLetter)
    {
      return false
    }
    return true
  }

  def isSet(s: String): Boolean =
  {
    if(s.isEmpty) return false
    val isFirstCharBracket = s.headOption.contains('{')
    if (!isFirstCharBracket)
    {
      return false
    }
    return true
  }

  def reversePolishToExpression(expression: String, varMap: Map[String, MultiSet[String]]): SetExpression =
  {
    val s: Stack[SetExpression] = new Stack()
    val elements = expression.split(" ")
    var i = 0
    while (i < elements.length)
    {
      if (isOperator(elements(i)))
      {
        val rhs = s.pop
        val lhs = s.pop
        val res = SetOperator(lhs, elements(i), rhs)
        s.push(res)
      }
      else if (isSet(elements(i)))
      {
        elements(i) = elements(i).drop(1).dropRight(1)
        val listOfElem: Array[String] = elements(i).split(",")
        val newSet: MultiSet[String] = MultiSet[String](listOfElem)
        val constantValue = SetConstant(newSet)
        s.push(constantValue)
      }
      else if (isVar(elements(i)))
      {
        if(varMap.contains(elements(i)))
        {
          val variableVal = varMap(elements(i))
          s.push(SetConstant(variableVal))
        }
        else
        {
          s.push(SetVar(elements(i)))
        }
      }
      else throw new Error("Unknown expression element " + elements(i))
      i += 1
    }
    s.top
  }


  private def isOperator(s: String) = s == "+" || s == "*" || s == "-"
}
