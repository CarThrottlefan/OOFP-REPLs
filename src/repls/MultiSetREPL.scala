package repls

import scala.collection.mutable
import scala.collection.mutable.{Queue, Stack}
import scala.util.matching.Regex

class MultiSetREPL extends REPLBase {
    // Have a REPL of a MutliSet of strings
    override type Base = MultiSet[String]
    override val replName: String = "multiset-repl"
    var globalMap: Map[String, MultiSet[String]] = Map()
    var operatorStack = Stack[String]()
    var outputQueue = Queue[String]()
    val letterPattern: Regex = "[a-zA-Z]+[0-9]*".r // only used to identify redundant characters, that are not part of the set

    override def readEval(command: String): String = {
        // TODO: complete me!
        var resultToString = ""
        val elements = command.split("\\s")
        //val elements = "@ {a} + {}".split("\\s")
        /*val queue = shuntingYard(elements)
        val expression = reversePolishToExpr(queue)
        val result = SetPatternMatch.SetEval(globalMap, expression)
        resultToString = result.toString
        resultToString*/

        if (elements.contains("=")) {
            val varName = elements.head
            /*if (!globalMap.contains(varName)) { //TODO FIX this for sets
                globalMap += (elements.head -> -1)
            }*/
            val queue = shuntingYard(elements.slice(2, elements.length))
            val expression = reversePolishToExpr(queue)
            //val result = expression.eval(globalMap)
            val result = SetPatternMatch.SetEval(globalMap, expression)
            globalMap = globalMap.updated(varName, result)
            resultToString = varName + " = " + result
        }
        else if (elements.head == "@") {
            val queue = shuntingYard((elements.slice(1, elements.length)))
            val expression = reversePolishToExpr(queue)
            val patternMatch = SetPatternMatch
            val simplified: SetExpression = patternMatch.SetSimplify(expression)
            //val result = patternMatch.eval(globalMap, simplified)
            resultToString = simplified.toString
            //val result = simplify()
        }
        else {
            val queue = shuntingYard(elements)
            val expression = reversePolishToExpr(queue)
            val result = SetPatternMatch.SetEval(globalMap, expression)
            resultToString = result.toString
        }
        resultToString
    }

    def shuntingYard(input: Array[String]): Queue[String] = {
        //val checkIfVar = "[a-zA-Z]+[0-9]*(?:\\s)+[+]".r
        for (i <- input.indices) {
            input(i) match {
                //left bracket - just push on stack

                //right bracket - pop till i find left bracket

                // case + or - => a case where something higher precedence is on stack, if not another case to just push

                // case * - push to stack

                case "(" =>
                    operatorStack.push(input(i))

                case ")" =>
                    while (operatorStack.top != "(") {
                        val topOfStack = operatorStack.pop()
                        outputQueue.enqueue(topOfStack)
                    }
                    operatorStack.pop() // pops the remaining left bracket from the stack

                case "*" =>
                    operatorStack.push(input(i))

                case "+" | "-" =>
                    if (operatorStack.isEmpty) {
                        operatorStack.push(input(i))
                    }

                    else {
                        while (operatorStack.nonEmpty && operatorStack.top == "*") {
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

        while (operatorStack.nonEmpty) {
            val topOfStack = operatorStack.pop()
            outputQueue.enqueue(topOfStack)
        }
        outputQueue
    }

    def reversePolishToExpr(input: mutable.Queue[String]): SetExpression = //repls.secondExpression =
    {
        var queueToString: String = ""
        for (i <- input.indices) {
            queueToString += input(i) + " "
        }
        queueToString = queueToString.trim //removes the additional whitespace at the end

        val expression: SetExpression = SetReversePolish.reversePolishToExpression(queueToString, globalMap)
        expression
    }

    // TODO: Implement any further functions that are specifically for an MultiSetREPL
}
