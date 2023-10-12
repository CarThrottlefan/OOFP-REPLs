package repls

case class Negate(arg: Expression) extends Expression {
  override def eval(bindings: Map[String, Int]): Int = -arg.eval(bindings)

  override def value: Int = -1
}
