package algo.sort

import algo.Util

/**
  * Created by serg on 14.07.16.
  */
object Inversions {


  def count(input: Array[Int]):Long = {

    val fromBuff = Array.ofDim[Int](input.length)
    val toBuff = Array.ofDim[Int](input.length)

    input.copyToArray(toBuff)

    def countAndSort(start:Int, end:Int):Long = {
      if (end - start <= 1) 0
      else {
        val mid = start + (end - start) / 2

        val leftN = countAndSort(start, mid)
        val rightN = countAndSort(mid, end)

        val splitN = mergeAndCountSplits(start, mid, end)

        leftN + rightN + splitN
      }
    }

    def mergeAndCountSplits(start:Int, mid:Int, end:Int):Long = {
      var n = 0L
      var i = start
      var j = mid
      Array.copy(toBuff, start, fromBuff, start, end - start)
      for(k <- start until end) {
        if (j >= end || (i < mid && fromBuff(i) <= fromBuff(j))) {
          toBuff(k) = fromBuff(i)
          i += 1
        } else {
          toBuff(k) = fromBuff(j)
          j += 1
          n += mid - i
        }
      }
      n
    }

    countAndSort(0, input.length)
  }

  def main(args: Array[String]) {
    import Util._
    val arr = readArrayFromFile[Int]("IntegerArray.txt")
    println(count(arr))

  }

}
