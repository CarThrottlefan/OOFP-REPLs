package repls

import scala.util.matching.Regex

// examples
// x
object SetPatternMatch {

  def SetOperatorByName(l: MultiSet[String], name: String, r: MultiSet[String]): MultiSet[String] = {
    var returnSet : MultiSet[String] = MultiSet.empty[String]

    name match {
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
    exp match {
      case SetConstant(i) => i
      case SetVar(s) if (bindings.contains(s)) => bindings(s)
      //case Var(s) if (!bindings.contains(s)) => s
      //case Negate(arg) => -eval(bindings, arg)
      case SetOperator(lhs, op, rhs) =>
        SetOperatorByName(SetEval(bindings, lhs), op, SetEval(bindings, rhs))
    }

  // rules :
  // -(-e) 	=> e
  // e + 0 => e
  // e * 1 => e
  def SetSimplify(exp: SetExpression): SetExpression =
    exp match {
      //case Negate(Negate(e)) => simplify(e)

      //case SetOperator(e, "-", e) => simplify(e)
      //case Negate(e) => Negate(simplify(e))

      case SetOperator(l, op, r) =>
        val bottomExp = SetOperator(SetSimplify(l), op, SetSimplify(r))

        bottomExp match
        {
          case SetOperator(SetConstant(value1), op, SetConstant(value2)) => SetConstant(SetOperatorByName(value1, op, value2))
          case SetOperator(e1, "*", e2) if e1 == e2 => SetSimplify(e1)
          case SetOperator(SetConstant(value), "*", e) if value.multiplicity.isEmpty =>
            val newSet = MultiSet.empty[String]
            SetConstant(newSet)
          case SetOperator(e, "*", SetConstant(value)) if value.multiplicity.isEmpty =>
            val newSet = MultiSet.empty[String]
            SetConstant(newSet)
          case SetOperator(SetConstant(value), "+", e) if value.multiplicity.isEmpty => SetSimplify(e)
          case SetOperator(e, "+", SetConstant(value)) if value.multiplicity.isEmpty => SetSimplify(e)
          case SetOperator(e1, "-", e2) if e1 == e2 =>
            val newSet = MultiSet.empty[String]
            SetConstant(newSet)
          //case SetOperator(e, "+", Constant(0)) => SetSimplify(e)
          //case SetOperator(Constant(0), "+", e) => SetSimplify(e)
          //case SetOperator(e, "*", Constant(1)) => SetSimplify(e)
          //case SetOperator(Constant(1), "*", e) => SetSimplify(e)
          //case SetOperator(e, "*", Constant(0)) => Constant(0)
          //case Operator(Constant(0), "*", e) => Constant(0)
          //case SetOperator(e1, "-", e2) if e1 == e2 => SetConstant(MultiSet[String](""))
          //case SetOperator(Operator(a1, "*", b), "+", Operator(a2, "*", c)) if a1 == a2 => SetSimplify(SetOperator(a1, "*", SetOperator(b, "+", c)))
          //case Operator(Operator(b, "*", a1), "+", Operator(a2, "*", c)) if a1 == a2 => SetSimplify(SetOperator(a1, "*", SetOperator(b, "+", c)))
          //case Operator(Operator(a1, "*", b), "+", Operator(c, "*", a2)) if a1 == a2 => SetSimplify(SetOperator(a1, "*", SetOperator(b, "+", c)))
          //case Operator(Operator(b, "*", a1), "+", Operator(c, "*", a2)) if a1 == a2 => SetSimplify(SetOperator(a1, "*", SetOperator(b, "+", c)))
          case _ => bottomExp
        }

      //case Constant(value) => Constant(value)
      case _ => exp
    }
}
