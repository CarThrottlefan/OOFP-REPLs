package repls

import scala.collection.mutable.Stack
import scala.util.matching.Regex

object SetReversePolish {

  def isVar(s: String): Boolean = {
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


  // converts reverse polish string to expression tree
  //
  // example :
  //   "1 3 5 + * 2 -"
  //   => SetOperator(SetOperator(Constant(1), "*", SetOperator(repls.Const(3), "+", repls.Const(5)), "-", repls.Const(2)))
  def reversePolishToExpression(expression: String, varMap: Map[String, MultiSet[String]]): SetExpression = {
    val s: Stack[SetExpression] = new Stack()
    /*for (el <- expression.split(" ")) {
      if (isOperator(el)) {
        val rhs = s.pop
        val lhs = s.pop
        val res = SetOperator(lhs, el, rhs)
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
        val res = SetOperator(lhs, elements(i), rhs)
        s.push(res)
      }
      else if (isSet(elements(i)))
      {
        elements(i) = elements(i).drop(1).dropRight(1)
        var listOfElem : Array[String] = elements(i).split(",")
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
