package repls
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.util.matching.Regex
import scala.collection.mutable.Queue


class IntREPL extends REPLBase {
    // Have a REPL of type Int
    type Base = Int
    override val replName: String = "int-repl"
    var globalMap: Map[String,Int] = Map()
    var operatorStack = Stack[String]()
    var outputQueue = Queue[String]()

    override def readEval(command: String): String = {
        val elements = command.split("\\s") // split string based on whitespace
        val queue = shuntingYard(elements)
        val expression = reversePolishFunc(queue)
        val result = expression.eval(globalMap)
        val resultToString = result.toString

        return resultToString

        // TODO: complete me!
        ""
        //a function that implements shunting yard algorithm, then create a tree that represents the formula given as input
    }
    // TODO: Implement any further functions that are specifically for an IntREPL
    def shuntingYard(input: Array[String]): Queue[String] =
    {
        val numberPattern: Regex = "[0-9]+".r // i am using this just to define if it is a num
        val letterPattern: Regex = "[a-zA-Z]+[0-9]*".r // i am using this just to define if it is a var

        for(i <- input.indices)
        {
            (input(i)) match
            {
                //left bracket - just push on stack

                //right bracket - pop till i find left bracket

                // case + or - => a case where something higher precedence is on stack, if not another case to just push

                // case * - push to stack

                case "(" =>
                    operatorStack.push(input(i))

                case ")" =>
                    while(operatorStack.top != "(")
                    {
                        val topOfStack = operatorStack.pop()
                        outputQueue.enqueue(topOfStack)
                    }
                    operatorStack.pop() // pops the remaining left bracket from the stack

                case "*" =>
                    operatorStack.push(input(i))

                case "+" | "-" =>
                    if(operatorStack.isEmpty)
                    {
                        operatorStack.push(input(i))
                    }

                    else
                    {
                        var topOperator = operatorStack.top
                        while(topOperator ==  "*" && operatorStack.nonEmpty)
                        {
                            topOperator = operatorStack.pop()
                            outputQueue.enqueue(topOperator)
                        }
                        operatorStack.push(input(i))
                    }

                case _ if (input(i).contains("-") && (numberPattern.findFirstMatchIn(input(i)).isDefined)) =>
                    val negate: Int = input(i).toInt
                    outputQueue.enqueue(negate.toString)

                case _ if numberPattern.findFirstMatchIn(input(i)).isDefined => //if it's a number
                    outputQueue.enqueue(input(i))

                case _ if letterPattern.findFirstMatchIn(input(i)).isDefined => //if it's a var
                    outputQueue.enqueue(input(i))
            }
        }

        while(operatorStack.nonEmpty)
        {
            val topOfStack = operatorStack.pop()
            outputQueue.enqueue(topOfStack)
        }
        outputQueue
    }

    def reversePolishFunc(input: mutable.Queue[String]): Expression  = //repls.secondExpression =
        {
            var queueToString : String = ""
            for(i <- input.indices)
            {
                queueToString += input(i) + " "
            }

            val expression : Expression = ReversePolish.reversePolishToExpression(queueToString)
            expression
        }
}
