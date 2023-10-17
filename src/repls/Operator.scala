package repls

case class Operator(lhs: Expression, operatorName: String, rhs: Expression) extends Expression {
  override def value: Int = operatorByName(operatorName, lhs.value, rhs.value)

  override def eval(bindings: Map[String, Int]): Int = {
    val l = lhs.eval(bindings)
    val r = rhs.eval(bindings)
    PatternMatch.operatorByName(l, operatorName, r).toInt
  }

  private def operatorByName(opName: String, lhs: Int, rhs: Int) =
  {
    if (opName == "+") lhs + rhs
    else if (opName == "*") lhs * rhs
    else lhs - rhs
  }

  override def toString: String =
     this match {
       case Operator(Operator(a, op1, b), operatorName, Operator(c, op2, d)) =>
          "( " + a.toString + " " + op1 + " " + b.toString + " ) " + operatorName + " ( " + c.toString + " " + op2 + " " + d.toString + " )"
       case Operator(Operator(a, op, b), operatorName, rhs) =>
         "( " + a.toString + " " + op + " " + b.toString + " ) " + operatorName + " " + rhs.toString
       case Operator(lhs, operatorName, Operator(c, op, d)) =>
         lhs.toString + " " + operatorName + " ( " + c.toString + " " + op + " " + d.toString + " )"
       case Operator(lhs, operatorName, rhs) =>
         lhs.toString  + " " + operatorName + " "  + rhs.toString
    }
}
