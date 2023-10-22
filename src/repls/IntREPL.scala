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
        var resultToString = ""
        val patternMatch = PatternMatch
        if(elements.contains("="))
        {
            val varName = elements.head
            if(!globalMap.contains(varName))
            {
                globalMap += (elements.head -> -1)
            }
            val queue = shuntingYard(elements.slice(2, elements.length))
            val expression = reversePolishToExpr(queue)
            val result = patternMatch.eval(globalMap, expression)
            globalMap = globalMap.updated(varName, result)
            resultToString = varName + " = " + result
        }
        else if (elements.head == "@")
        {
            val queue = shuntingYard((elements.slice(1, elements.length)))
            val expression = reversePolishToExpr(queue)
            val patternMatch = PatternMatch
            val simplified: Expression = patternMatch.simplify(expression)
            resultToString = simplified.toString
        }
        else
        {
            val queue = shuntingYard(elements)
            val expression = reversePolishToExpr(queue)
            val result = patternMatch.eval(globalMap, expression)
            resultToString = result.toString
        }
        return resultToString
    }
    def shuntingYard(input: Array[String]): Queue[String] =
    {
        val numberPattern: Regex = "[0-9]+".r // i am using this just to define if it is a num
        val letterPattern: Regex = "[a-zA-Z]+[0-9]*".r // i am using this just to define if it is a var

        for(i <- input.indices)
        {
            input(i) match
            {
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
                        while(operatorStack.nonEmpty && operatorStack.top ==  "*")
                        {
                            outputQueue.enqueue(operatorStack.top)
                            operatorStack.pop()
                        }
                        operatorStack.push(input(i))
                    }

                case _ if input(i).contains("-") && numberPattern.findFirstMatchIn(input(i)).isDefined =>
                    val negate: Int = input(i).toInt
                    outputQueue.enqueue(negate.toString)

                case _ if numberPattern.findFirstMatchIn(input(i)).isDefined => //if it's a number
                    outputQueue.enqueue(input(i))

                case _ if letterPattern.findFirstMatchIn(input(i)).isDefined => //if it's a var
                    if(globalMap.contains(input(i)))
                    {
                        outputQueue.enqueue(globalMap(input(i)).toString)
                    }
                    else
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

    def reversePolishToExpr(input: mutable.Queue[String]): Expression  =
    {
        var queueToString : String = ""
        for(i <- input.indices)
        {
            queueToString += input(i) + " "
        }
        queueToString = queueToString.trim //removes the additional whitespace at the end

        val expression : Expression = ReversePolish.reversePolishToExpression(queueToString, globalMap)
        expression
    }
}
