package repls

case class Constant(n: Int) extends Expression {
  override def value: Int = n
  override def eval(bindings: Map[String, Int]): Int = n

  override def toString: String = n.toString
}
