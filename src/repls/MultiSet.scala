package repls

import repls.MultiSet.empty

/*
    Multiset is a Map of elements and their respective count.
    For example:
    {a,a,a,b,c,c} = Map('a'->3, 'b'->1, 'c'->2)
 */


case class MultiSet[T] (multiplicity: Map[T, Int])
{
    def *(that: MultiSet[T]): MultiSet[T] =
    {
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

    def +(that: MultiSet[T]): MultiSet[T] =
    {
        var returnMap: Map[T, Int] = Map()
        multiplicity.foreach
        {
            case(elements, valueMap1) => returnMap += (elements -> valueMap1)
        }
        that.multiplicity.foreach
        {
            case(elements, valueMap2) =>
                if(elements.toString != "" && valueMap2 > 0)
                {
                    if(returnMap.contains(elements))
                        returnMap = returnMap.updated(elements, returnMap(elements) + valueMap2)
                    else
                        returnMap += (elements -> valueMap2)
                }
        }
        val set = new MultiSet(returnMap)
        set
    }
    def -(that: MultiSet[T]): MultiSet[T] =
    {
        var returnMap: Map[T, Int] = Map()
        multiplicity.foreach
        {
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

    def toSeq: Seq[T] =
    {
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

    override def toString: String =
    {
        def elemToString(elem: T): String =
        {
            val count = multiplicity(elem)
            if (count >= MaxCountForDuplicatePrint)
                elem.toString + " -> " + count.toString
            else Seq.fill(count)(elem).mkString(",")
        }

        val keyStringSet = multiplicity.collect
        {
            case (elem, count) if count > 0 => elemToString(elem)
        }
        var keyStringToSeq = keyStringSet.toSeq

        if(keyStringSet.toSeq.contains("")) //final check to see if there is no empty element in the set
            keyStringToSeq = keyStringSet.toSeq.filterNot(_ == "")

        "{" + keyStringToSeq.sorted.mkString(",") + "}"
    }
}

object MultiSet {
    def empty[T] : MultiSet[T] = MultiSet(Map[T,Int]())

    def apply[T](elements: Seq[T]): MultiSet[T] =
    {
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
