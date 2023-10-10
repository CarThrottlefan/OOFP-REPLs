package repls
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.util.matching.Regex
import scala.collection.mutable.Queue

class IntREPL extends REPLBase {
    // Have a REPL of type Int
    type Base = Int
    override val replName: String = "int-repl"

    override def readEval(command: String): String = {
        val elements = command.split("\\s") // split string based on whitespace
        shuntingYard(elements)
        // TODO: complete me!
        ""
        //a function that implements shunting yard algorithm, then create a tree that represents the formula given as input
    }
    // TODO: Implement any further functions that are specifically for an IntREPL
    def shuntingYard(input: Array[String]): Queue[String] =
    {
        val numberPattern: Regex = "[0-9]+".r // i am using this just to define if it is a num
        val letterPattern: Regex = "[a-zA-Z]+[0-9]*".r // i am using this just to define if it is a var
        var operatorStack = Stack[String]()
        var outputQueue = Queue[String]()

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
                    var topOperator = operatorStack.pop()
                    while(topOperator == "(" || topOperator ==  "*")
                    {
                        outputQueue.enqueue(topOperator)
                        topOperator = operatorStack.pop()
                    }
                    operatorStack.push(input(i))

                case _ if numberPattern.findFirstMatchIn(input(i)).isDefined => //if it's a number
                    outputQueue.enqueue(input(i))

                case _ if letterPattern.findFirstMatchIn(input(i)).isDefined => //if it's a var
            }
        }

        while(operatorStack.nonEmpty)
        {
            val topOfStack = operatorStack.pop()
            outputQueue.enqueue(topOfStack)
        }
        outputQueue
    }
}
