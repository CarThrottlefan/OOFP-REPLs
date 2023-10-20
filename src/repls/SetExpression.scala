package repls

abstract class SetExpression {
  def value: MultiSet[String]
  def eval(bindings : Map[String,MultiSet[String]]) : MultiSet[String]

  def describe: String =
    "The value of " + toString + " is " + value.toString

  def toString : String
}