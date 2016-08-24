package algo.dynprog

import scala.io.Source

/**
  * Created by serg on 21.08.16.
  */
object Knapsack {

  case class Item(value:Int, size:Int)

  def optimal(items:Seq[Item], size:Int):Int = {
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

  def readItems(fileName:String):Seq[Item] = {
    (for(line <- Source.fromFile(fileName).getLines()) yield {
      val attr = line.split("\\s+")
      Item(attr(0).toInt, attr(1).toInt)
    }).toSeq
  }

  def main(args: Array[String]): Unit = {
    val items = readItems("knapsack1.txt")
    println(optimal(items.tail, items.head.value) == 2493893)
  }

}
