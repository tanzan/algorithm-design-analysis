package algo.dynprog

import scala.collection.mutable
import scala.io.Source

/**
  * Created by serg on 21.08.16.
  */
object Knapsack {

  case class Item(value:Int, size:Int)

  def optimalUp(items:Seq[Item], size:Int):Int = {
    val solutions = Array.ofDim[Int](items.size + 1, size + 1)
    for(i <- 1 to items.size) {
      for(j <- 0 to size) {
        val w = j - items(i - 1).size
        if (w < 0) solutions(i)(j) = solutions(i - 1)(j)
        else solutions(i)(j) = Math.max(solutions(i - 1)(j), solutions(i - 1)(w) + items(i - 1).value)
      }
    }
    solutions(items.size)(size)
  }

  case class Solution(prefix:Int, size:Int)

  def optimalDown(items:Seq[Item], size:Int):Int = {

    val solutions = mutable.Map[Solution, Int]()

    def compute(prefix:Int, size:Int):Int = {
      val s = Solution(prefix, size)
      if (solutions.contains(s)) solutions(s)
      else if (prefix < 0) 0
      else {
        val v1 = compute(prefix - 1, size)
        val v2 = if (size - items(prefix).size < 0) v1
        else compute(prefix - 1, size - items(prefix).size) + items(prefix).value

        val v = Math.max(v1, v2)
        solutions += Solution(prefix, size) -> v
        v
      }
    }
    compute(items.size - 1, size)
  }

  def readItems(fileName:String):Seq[Item] = {
    (for(line <- Source.fromFile(fileName).getLines()) yield {
      val attr = line.split("\\s+")
      Item(attr(0).toInt, attr(1).toInt)
    }).toSeq
  }

  def main(args: Array[String]): Unit = {
    val items = readItems("knapsack1.txt")
    println(optimalUp(items.tail, items.head.value) == 2493893)

    val bibItems = readItems("knapsack_big.txt")
    println(optimalDown(bibItems.tail, bibItems.head.value) == 4243395)
  }

}
