package repls

case class SetOperator(lhs: SetExpression, operatorName: String, rhs: SetExpression) extends SetExpression
{
  override def value: MultiSet[String] = operatorByName(operatorName, lhs.value, rhs.value)

  override def eval(bindings: Map[String, MultiSet[String]]): MultiSet[String] =
  {
    val l = lhs.eval(bindings)
    val r = rhs.eval(bindings)
    SetPatternMatch.SetOperatorByName(l, operatorName, r)
  }

  private def operatorByName(opName: String, lhs: MultiSet[String], rhs: MultiSet[String]): MultiSet[String] =
  {
    if (opName == "+") lhs + rhs
    else if (opName == "*") lhs * rhs
    else lhs - rhs
  }

  override def toString: String =
    this match
    {
      case SetOperator(SetOperator(a, op1, b), operatorName, SetOperator(c, op2, d)) =>
        if((op1 == "+" || op1 == "-") && (op2 == "+" || op2 == "-") && operatorName == "*")
        {
          "( " + a.toString + " " + op1 + " " + b.toString + " ) " + operatorName + " ( " + c.toString + " " + op2 + " " + d.toString + " )"
        }
        else if (operatorName == "*"  && (op2 == "+"  || op2 == "-"))
        {
          a.toString + " " + op1 + " " + b.toString + " " + operatorName + " ( " + c.toString + " " + op2 + " " + d.toString + " )"
        }
        else
        {
          a.toString + " " + op1 + " " + b.toString + " " + operatorName + " " + c.toString + " " + op2 + " " + d.toString
        }
      case SetOperator(SetOperator(a, op, b), operatorName, rhs) if (op == "+" || op == "-") && (operatorName == "*") =>
        "( " + a.toString + " " + op + " " + b.toString + " ) " + operatorName + " " + rhs.toString
      case SetOperator(lhs, operatorName, SetOperator(c, op, d)) if (op == "+" || op == "-") && (operatorName == "*") =>
        lhs.toString + " " + operatorName + " ( " + c.toString + " " + op + " " + d.toString + " )"
      case _ =>
        lhs.toString  + " " + operatorName + " "  + rhs.toString
    }
}
