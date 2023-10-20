package repls

case class SetVar(s: String) extends SetExpression {
  override def eval(bindings: Map[String, MultiSet[String]]): MultiSet[String] =
  {
    //Int = bindings(s
    var bindingsCopy = bindings
    val newValue = _defMap(s)
    bindingsCopy = bindings.updated(s, newValue)
    bindingsCopy(s)
  }
  private var _value : MultiSet[String] = MultiSet.empty[String]
  private var _defMap : Map[String, MultiSet[String]] = Map()
  //private var listOfUpdates : List[String] = List()
  override def value: MultiSet[String] = _value

  def newValue(newVal: MultiSet[String], key: String): Unit = {
    _value = newVal
    //bindings + (key -> newVal)
    //listOfUpdates = listOfUpdates :+ key
    _defMap = _defMap + (key -> newVal)
  }

  override def toString : String =
  {
    s
  }
}
