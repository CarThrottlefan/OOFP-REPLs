package repls

import scala.util.matching.Regex

// examples
// x
object PatternMatch {

  def operatorByName(l: String, name: String, r: String): String = {
    val numberPattern: Regex = "[0-9]+".r
    var returnString : String = ""

    name match {
      case "+" =>
        if (numberPattern.findFirstMatchIn(l).isDefined && numberPattern.findFirstMatchIn(r).isDefined) // l and r are numbers -> returns a numerical value
        {
          returnString = (l.toInt + r.toInt).toString
          returnString
        }
        else // returns a string containing the unbounded var
        {
          returnString = l + " + " + r
          returnString
        }
      case "-" =>
        if (numberPattern.findFirstMatchIn(l).isDefined && numberPattern.findFirstMatchIn(r).isDefined)
        {
          returnString = (l.toInt - r.toInt).toString
          returnString
        }
        else
        {
          returnString = l + " - " + r
          returnString
        }
      case "*" =>
        if (numberPattern.findFirstMatchIn(l).isDefined && numberPattern.findFirstMatchIn(r).isDefined) // l and r are numbers
        {
          returnString = (l.toInt * r.toInt).toString
          returnString
        }
        else
        {
          returnString = l + " * " + r
          returnString
        }
    }
  }

  def eval(bindings: Map[String, Int], exp: Expression): String =
    exp match {
      case Constant(i) => i.toString
      case Var(s) if (bindings.contains(s)) => bindings(s).toString
      case Var(s) if (!bindings.contains(s)) => s
      //case Negate(arg) => -eval(bindings, arg)
      case Operator(lhs, op, rhs) =>
        operatorByName(eval(bindings, lhs), op, eval(bindings, rhs))
    }

  // rules :
  // -(-e) 	=> e
  // e + 0 => e
  // e * 1 => e
  def simplify(exp: Expression): Expression =
    exp match {
      //case Negate(Negate(e)) => simplify(e)
      case Operator(e, "+", Constant(0)) => simplify(e)
      case Operator(Constant(0), "+", e) => simplify(e)
      case Operator(e, "*", Constant(1)) => simplify(e)
      case Operator(Constant(1), "*", e) => simplify(e)
      case Operator(e, "*", Constant(0)) => Constant(0)
      case Operator(Constant(0), "*", e) => Constant(0)
      case Operator(e1, "-", e2) if (e1 == e2) => Constant(0)
      case Operator(Operator(a1, "*", b), "+", Operator(a2, "*", c)) if (a1 == a2) => Operator(a1, "*", Operator(b, "+", c))
      case Operator(Operator(b, "*", a1), "+", Operator(a2, "*", c)) if (a1 == a2) => Operator(a1, "*", Operator(b, "+", c))
      case Operator(Operator(a1, "*", b), "+", Operator(c, "*", a2)) if (a1 == a2) => Operator(a1, "*", Operator(b, "+", c))
      case Operator(Operator(b, "*", a1), "+", Operator(c, "*", a2)) if (a1 == a2) => Operator(a1, "*", Operator(b, "+", c))
      //case Operator(e, "-", e) => simplify(e)
      //case Negate(e) => Negate(simplify(e))
      case Operator(l, op, r) => Operator(simplify(l), op, simplify(r))
      case _ => exp
    }
}
