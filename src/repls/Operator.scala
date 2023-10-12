package repls

case class Operator(lhs: Expression, operatorName: String, rhs: Expression) extends Expression {
  override def value: Int = operatorByName(operatorName, lhs.value, rhs.value)

  override def eval(bindings: Map[String, Int]): Int = {
    val l = lhs.eval(bindings)
    val r = rhs.eval(bindings)
    PatternMatch.operatorByName(l, operatorName, r)
  }

  private def operatorByName(opName: String, lhs: Int, rhs: Int) = {
    if (opName == "+") lhs + rhs
    else if (opName == "*") lhs * rhs
    else lhs - rhs
  }

  override def toString: String = "(" + lhs.toString + operatorName + rhs.toString + ")"
}
