package algo.struct

import algo.Util

import scala.collection.immutable.HashSet

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


  def main(args: Array[String]): Unit = {
    import Util._
    val hashSet = HashSet(Util.readArrayFromFile[Long]("2sum.txt"):_*)
    val hasSum = for{
      t <- -10000 to 10000
      x <- hashSet if hashSet.contains(t - x) && (t -x) != x
    } yield {
      println(t)
      t
    }
    println(hasSum.size)
  }
}
