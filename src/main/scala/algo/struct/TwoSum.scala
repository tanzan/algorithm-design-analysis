package algo.struct

import algo.Util

/**
  * Created by serg on 8/9/16.
  */
object TwoSum {

  /*The goal of this problem is to implement a variant of the 2-SUM algorithm (covered in the Week 6 lecture on hash table applications).

    The file contains 1 million integers, both positive and negative (there might be some repetitions!).This is your array of integers, with the ith row of the file specifying the ith entry of the array.

    Your task is to compute the number of target values t in the interval [-10000,10000] (inclusive) such that there are distinct numbers x,y in the input file that satisfy x+y=t.
    (NOTE: ensuring distinctness requires a one-line addition to the algorithm from lecture.)

    Write your numeric answer (an integer between 0 and 20001) in the space provided.
   */

  @inline
  def hasSum(t:Int, array: Array[Long], hash: Map[Long, Long]):Boolean = {
    var i = 0
    while(i < array.length) {
      val y = t - array(i)
      if (hash.contains(y) && y != array(i)){
        return true
      }
      i += 1
    }
    false
  }

  def main(args: Array[String]): Unit = {
    import Util._
    val array = Util.readArrayFromFile[Long]("2sum.txt")
    val hash = array.map(x => x -> x).toMap
    val sums = for(t <- (-10000 to 10000).par) yield hasSum(t, array, hash)
    println(sums.count(x => x) == 427)
  }
}
