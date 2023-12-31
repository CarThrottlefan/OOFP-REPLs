package repls

import scala.collection.mutable
import scala.collection.mutable.{Queue, Stack}
import scala.util.matching.Regex

class MultiSetREPL extends REPLBase {
    override type Base = MultiSet[String]
    override val replName: String = "multiset-repl"
    var globalMap: Map[String, MultiSet[String]] = Map()
    var operatorStack = Stack[String]()
    var outputQueue = Queue[String]()
    val letterPattern: Regex = "[a-zA-Z]+[0-9]*".r // only used to identify redundant characters, that are not part of the set

    override def readEval(command: String): String =
    {
        var resultToString = ""
        val elements = command.split("\\s")

        if (elements.contains("="))
        {
            val varName = elements.head
            if (!globalMap.contains(varName))
            {
                globalMap += (elements.head -> MultiSet.empty[String])
            }
            val queue = shuntingYard(elements.slice(2, elements.length))
            val expression = reversePolishToExpr(queue)
            val result = SetPatternMatch.SetEval(globalMap, expression)
            globalMap = globalMap.updated(varName, result)
            resultToString = varName + " = " + result
        }
        else if (elements.head == "@")
        {
            val queue = shuntingYard((elements.slice(1, elements.length)))
            val expression = reversePolishToExpr(queue)
            val patternMatch = SetPatternMatch
            val simplified: SetExpression = patternMatch.SetSimplify(expression, globalMap)
            resultToString = simplified.toString
        }
        else
        {
            val queue = shuntingYard(elements)
            val expression = reversePolishToExpr(queue)
            val result = SetPatternMatch.SetEval(globalMap, expression)
            resultToString = result.toString
        }
        resultToString
    }

    def shuntingYard(input: Array[String]): Queue[String] =
    {
        for (i <- input.indices)
        {
            input(i) match
            {
                case "(" =>
                    operatorStack.push(input(i))

                case ")" =>
                    while (operatorStack.top != "(")
                    {
                        val topOfStack = operatorStack.pop()
                        outputQueue.enqueue(topOfStack)
                    }
                    operatorStack.pop() // pops the remaining left bracket from the stack

                case "*" =>
                    operatorStack.push(input(i))

                case "+" | "-" =>
                    if (operatorStack.isEmpty)
                    {
                        operatorStack.push(input(i))
                    }

                    else
                    {
                        while (operatorStack.nonEmpty && operatorStack.top == "*")
                        {
                            outputQueue.enqueue(operatorStack.top)
                            operatorStack.pop()
                        }
                        operatorStack.push(input(i))
                    }
                case _ if letterPattern.findFirstMatchIn(input(i)).isDefined && !input(i).contains(',') => //if it's a var
                    if (globalMap.contains(input(i)))
                    {
                        outputQueue.enqueue(globalMap(input(i)).toString)
                    }
                    else
                        outputQueue.enqueue(input(i))

                case _ =>
                    outputQueue.enqueue(input(i))
            }
        }

        while (operatorStack.nonEmpty)
        {
            val topOfStack = operatorStack.pop()
            outputQueue.enqueue(topOfStack)
        }
        outputQueue
    }

    def reversePolishToExpr(input: mutable.Queue[String]): SetExpression =
    {
        var queueToString: String = ""
        for (i <- input.indices)
        {
            queueToString += input(i) + " "
        }
        queueToString = queueToString.trim //removes the additional whitespace at the end

        val expression: SetExpression = SetReversePolish.reversePolishToExpression(queueToString, globalMap)
        expression
    }
}
