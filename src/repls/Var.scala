package repls

case class Var(s: String) extends Expression
{
  override def eval(bindings: Map[String, Int]): Int =
  {
    var bindingsCopy = bindings
    val newValue = _defMap(s)
    bindingsCopy = bindings.updated(s, newValue)
    bindingsCopy(s)
  }
  private var _value : Int = -1
  private var _defMap : Map[String, Int] = Map()
  override def value: Int = _value

  def newValue(newVal: Int, key: String): Unit =
  {
    _value = newVal
    _defMap = _defMap + (key -> newVal)
  }

  override def toString : String =
    {
      s
    }
}
