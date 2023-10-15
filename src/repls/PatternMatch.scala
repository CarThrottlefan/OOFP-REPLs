package repls

// examples
// x
object PatternMatch {

  def operatorByName(l: Int, name: String, r: Int): Int = {
    name match {
      case "+" => l + r
      case "-" => l - r
      case "*" => l * r
      case "/" => l / r
    }
  }

  def eval(bindings: Map[String, Int], exp: Expression): String =
    exp match {
      case Constant(i) => i.toString
      case Var(s) if (bindings.contains(s)) => bindings(s).toString
      case Var(s) if (!bindings.contains(s)) => s
      //case Negate(arg) => -eval(bindings, arg)
      case Operator(lhs, op, rhs) =>
        operatorByName(eval(bindings, lhs).toInt, op, eval(bindings, rhs).toInt).toString
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
      case Operator(e, "*", Constant(0)) => simplify(e)
      case Operator(Constant(0), "*", e) => simplify(e)
      //case Operator(e, "-", e) => simplify(e)
      //case Negate(e) => Negate(simplify(e))
      case Operator(l, op, r) => Operator(simplify(l), op, simplify(r))
      case _ => exp
    }
}
