package repls

case class Var(s: String) extends Expression {
  override def eval(bindings: Map[String, Int]): Int = bindings(s)

  override def value: Int = -1
}
