package repls

import scala.util.matching.Regex

object SetPatternMatch {

  def SetOperatorByName(l: MultiSet[String], name: String, r: MultiSet[String]): MultiSet[String] =
  {
    var returnSet : MultiSet[String] = MultiSet.empty[String]

    name match
    {
      case "+" =>
        returnSet = l + r
        returnSet
      case "-" =>
        returnSet = l - r
        returnSet
      case "*" =>
        returnSet = l * r
        returnSet
    }
  }

  def SetEval(bindings: Map[String, MultiSet[String]], exp: SetExpression): MultiSet[String] =
    exp match
    {
      case SetConstant(i) => i
      case SetVar(s) if (bindings.contains(s)) => bindings(s)
      case SetOperator(lhs, op, rhs) =>
        var newSet = MultiSet.empty[String]
        newSet = SetOperatorByName(SetEval(bindings, lhs), op, SetEval(bindings, rhs))
        newSet
    }

  def SetSimplify(exp: SetExpression, bindings: Map[String, MultiSet[String]]): SetExpression =
    exp match
    {
      case SetOperator(l, op, r) =>
        val bottomExp = SetOperator(SetSimplify(l, bindings), op, SetSimplify(r, bindings))

        bottomExp match
        {
          case SetOperator(e1, "*", e2) if e1 == e2 => SetSimplify(e1, bindings)

          case SetOperator(SetConstant(value), "*", e) if value.multiplicity.size == 1 && value.multiplicity.contains("") => //basically if the set is empty
            val newSet = MultiSet.empty[String]
            SetConstant(newSet)

          case SetOperator(e, "*", SetConstant(value)) if value.multiplicity.size == 1 && value.multiplicity.contains("") =>
            val newSet = MultiSet.empty[String]
            SetConstant(newSet)

          case SetOperator(SetConstant(value), "+", e) if value.multiplicity.size == 1 && value.multiplicity.contains("") => SetSimplify(e, bindings)
          case SetOperator(e, "+", SetConstant(value)) if value.multiplicity.size == 1 && value.multiplicity.contains("") => SetSimplify(e, bindings)

          case SetOperator(e1, "-", e2) if e1 == e2 =>
            val newSet = MultiSet.empty[String]
            SetConstant(newSet)

          case SetOperator(SetConstant(value1), op, SetConstant(value2)) => SetConstant(SetOperatorByName(value1, op, value2))
          case _ => bottomExp
        }
      case SetVar(name) =>
        bindings.get(name) match
        {
          case Some(value) => SetConstant(value)
          case None => SetVar(name)
        }
      case _ => exp
    }
}
