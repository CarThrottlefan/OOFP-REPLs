package repls

case class Var(s: String) extends Expression {
  override def eval(bindings: Map[String, Int]): Int =
  {
    //Int = bindings(s
    var bindingsCopy = bindings
    val newValue = _defMap(s)
    bindingsCopy = bindings.updated(s, newValue)
    bindingsCopy(s)
  }
  private var _value : Int = -1
  private var _defMap : Map[String, Int] = Map()
  //private var listOfUpdates : List[String] = List()
  override def value: Int = _value

  def newValue(newVal: Int, key: String): Unit = {
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
