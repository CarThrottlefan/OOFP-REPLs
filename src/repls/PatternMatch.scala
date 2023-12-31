package repls

import scala.util.matching.Regex

object PatternMatch {

  def operatorByName(l: Int, name: String, r: Int): Int = {
    var returnInt : Int = 0

    name match
    {
      case "+" =>
          returnInt = l + r
          returnInt
      case "-" =>
          returnInt = l - r
          returnInt
      case "*" =>
          returnInt = l * r
          returnInt
    }
  }

  def eval(bindings: Map[String, Int], exp: Expression): Int =
    exp match
    {
      case Constant(i) => i
      case Var(s) if (bindings.contains(s)) => bindings(s)
      case Operator(lhs, op, rhs) =>
        operatorByName(eval(bindings, lhs), op, eval(bindings, rhs))
    }
  def simplify(exp: Expression): Expression =
    exp match
    {
      case Operator(l, op, r) =>
        val bottomExp = Operator(simplify(l), op, simplify(r))

        bottomExp match
        {
          case Operator(Constant(value1), op, Constant(value2)) => Constant(operatorByName(value1, op, value2))
          case Operator(e, "+", Constant(0)) => simplify(e)
          case Operator(Constant(0), "+", e) => simplify(e)
          case Operator(e, "*", Constant(1)) => simplify(e)
          case Operator(Constant(1), "*", e) => simplify(e)
          case Operator(e, "*", Constant(0)) => Constant(0)
          case Operator(Constant(0), "*", e) => Constant(0)
          case Operator(e1, "-", e2) if e1 == e2 => Constant(0)
          case Operator(Operator(a1, "*", b), "+", Operator(a2, "*", c)) if a1 == a2 => simplify(Operator(a1, "*", Operator(b, "+", c)))
          case Operator(Operator(b, "*", a1), "+", Operator(a2, "*", c)) if a1 == a2 => simplify(Operator(a1, "*", Operator(b, "+", c)))
          case Operator(Operator(a1, "*", b), "+", Operator(c, "*", a2)) if a1 == a2 => simplify(Operator(a1, "*", Operator(b, "+", c)))
          case Operator(Operator(b, "*", a1), "+", Operator(c, "*", a2)) if a1 == a2 => simplify(Operator(a1, "*", Operator(b, "+", c)))
          case _ => bottomExp
        }
      case _ => exp
    }
}
