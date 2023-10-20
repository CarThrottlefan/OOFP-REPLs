package repls

case class SetConstant(n: MultiSet[String]) extends SetExpression {


  //var set: MultiSet[String] = MultiSet(n)
  override def value: MultiSet[String] = n

  override def eval(bindings: Map[String, MultiSet[String]]): MultiSet[String] = n

  override def toString: String = n.toString
}
