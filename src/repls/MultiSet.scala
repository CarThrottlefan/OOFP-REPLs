package repls

import repls.MultiSet.empty

/*
    Multiset is a Map of elements and their respective count.
    For example:
    {a,a,a,b,c,c} = Map('a'->3, 'b'->1, 'c'->2)
 */


case class MultiSet[T] (multiplicity: Map[T, Int]) {

    /* TODO
        Intersection of two multisets:
        ∀x m_c(x) = min(m_a(x), m_b(x))
        Example:
        {a,b,b,c,c,c} * {b,c,c,c,c} = {b,c,c,c}
     */
    def *(that: MultiSet[T]): MultiSet[T] = {
        var returnMap: Map[T, Int] = Map()
        multiplicity.foreach
        {
            case(elements, valueMap1) =>
            if(that.multiplicity.contains(elements))
            {
            val valueMap2 = that.multiplicity(elements)
            val min : Int = valueMap1.min(valueMap2)
            returnMap += (elements -> min)
            if(returnMap(elements) == 0)
                returnMap = returnMap.removed(elements)
            }
        }
        val set = new MultiSet(returnMap)
        set
    }
    /* TODO
        Summation of two multisets:
        ∀x m_c(x) = m_a(x) + m_b(x)
        Example:
        {a,b,c,c} + {a,c,d} = {a,a,b,c,c,c,d}
     */

    def +(that: MultiSet[T]): MultiSet[T] = {
    /* TODO
        Subtraction of two multisets:
        ∀x m_c(x) = max(m_a(x) - m_b(x), 0)
        Example:
        {a,b,b,d} - {b,c,c,d,d} = {a,b}
     */
    var returnMap: Map[T, Int] = Map()
    multiplicity.foreach
    {
        case(elements, valueMap1) => returnMap += (elements -> valueMap1)
    }
    that.multiplicity.foreach
    {
        case(elements, valueMap2) =>
            if(returnMap.contains(elements))
                returnMap = returnMap.updated(elements, returnMap(elements) + valueMap2)
            else
                returnMap += (elements -> valueMap2)
    }
    val set = new MultiSet(returnMap)
    set
}
    def -(that: MultiSet[T]): MultiSet[T] =
    {
        var returnMap: Map[T, Int] = Map()
        multiplicity.foreach {
            case (elements, valueMap1) =>
                if(that.multiplicity.contains(elements))
                {
                    val valueMap2 = that.multiplicity(elements)
                    val max = (valueMap1 - valueMap2).max(0)
                    returnMap += (elements -> max)
                    if(returnMap(elements) == 0)
                        returnMap = returnMap.removed(elements)
                }
                else
                {
                    returnMap += (elements -> valueMap1)
                }
        }
        val set = new MultiSet(returnMap)
        set
    }
    /* TODO
        Make sure a multiset can be returned as a sequence.

        For example the multiset {a,a,b} should give the sequence Seq(a,a,b).

        The order of the elements in the sequence does not matter.
     */
    def toSeq: Seq[T] = {
        var retSeq: Seq[T] = Seq()
        multiplicity.foreach{
            case (element, count) =>
                for(i <- 1 to count)
                {
                    retSeq = retSeq :+ element
                }
        }
        retSeq
    }

    val MaxCountForDuplicatePrint = 5

    // A toString has already been provided
    override def toString: String = {
        def elemToString(elem: T): String = {
            val count = multiplicity(elem)
            if (count >= MaxCountForDuplicatePrint)
                elem.toString + " -> " + count.toString
            else Seq.fill(count)(elem).mkString(",")
        }

        val keyStringSet = multiplicity.keySet.map(elemToString)
        "{" + keyStringSet.toSeq.sorted.mkString(",") + "}"
    }
}

object MultiSet {
    def empty[T] : MultiSet[T] = MultiSet(Map[T,Int]())
    /* TODO
        Write a constructor that constructs a multiset from a sequence of elements
     */
    def apply[T](elements: Seq[T]): MultiSet[T] = {
        var keyList:Map[T,Int] = Map();
        elements.foreach(elements =>
        {
            if(keyList.contains(elements))
            {
                var currVal = keyList(elements)
                currVal += 1
                keyList = keyList.updated(elements, currVal)
            }
            else
            {
                keyList = keyList + (elements -> 1)
            }
        })
        val set = new MultiSet(keyList)
        set
    }
}
