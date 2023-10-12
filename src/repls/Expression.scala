package repls

abstract class Expression {
  def value: Int
  def eval(bindings : Map[String,Int]) : Int

  def describe: String =
    "The value of " + toString + " is " + value.toString
}

