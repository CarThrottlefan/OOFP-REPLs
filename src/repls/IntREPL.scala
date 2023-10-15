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
    //var variableMap: Map[String, Int] = Map()
    var newVar : Boolean = false

    override def readEval(command: String): String = {
        val elements = command.split("\\s") // split string based on whitespace //TODO outcomment this for the normal functioning
        //val elements = "n = 1".split("\\s")
        var resultToString = ""
        if(elements.contains("="))
        {
            val varName = elements.head
            globalMap += (elements.head -> -1)
            val queue = shuntingYard(elements.slice(2, elements.length))
            val expression = reversePolishToExpr(queue)
            val result = expression.eval(globalMap)
            globalMap = globalMap.updated(varName, result)
            resultToString = varName + " = " + result
        }
        else if (elements.head == ("@"))
        {
            val queue = shuntingYard((elements.slice(1, elements.length)))
            val expression = reversePolishToExpr(queue)
            //val result = simplify()

        }
        else
        {
            val queue = shuntingYard(elements)
            val expression = reversePolishToExpr(queue)
            val result = expression.eval(globalMap)
            resultToString = result.toString
        }

        /*val queue = shuntingYard(elements)
        val expression = reversePolishToExpr(queue)
        val result = expression.eval(globalMap)*/
        /*val resultToString: String =
            if (globalMap == Map())
                {
                    result.toString
                }
            else
                {
                    var newString : String = ""
                    globalMap = globalMap.updated(elements(0), result)
                    newString = elements(0) + " = " + result.toString
                    newString
                }*/

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
            input(i) match
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
                    //outputQueue.enqueue(input(i))
                    outputQueue.enqueue(globalMap.get(input(i)).toString)

                /*case "@" =>
                    outputQueue.enqueue(input(i))*/
            }
        }

        while(operatorStack.nonEmpty)
        {
            val topOfStack = operatorStack.pop()
            outputQueue.enqueue(topOfStack)
        }
        outputQueue
    }

    def reversePolishToExpr(input: mutable.Queue[String]): Expression  = //repls.secondExpression =
        {
            var queueToString : String = ""
            for(i <- input.indices)
            {
                queueToString += input(i) + " "
            }
            queueToString = queueToString.trim //removes the additional whitespace at the end

            val expression : Expression = ReversePolish.reversePolishToExpression(queueToString)
            expression
        }
}
