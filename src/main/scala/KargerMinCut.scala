import scala.io.Source
import scala.util.Random

/**
  * Created by serg on 7/22/16.
  */
object KargerMinCut {

  val random = new Random()

  def readFromFile(fileName:String):Map[Int, List[Int]] = {
    Source.fromFile(fileName).getLines().map(line => {
      val vertices = line.split("\\s+").map(_.trim.toInt).toList
      vertices.head -> vertices.tail
    }).toMap
  }

  def minCut(graph:Map[Int, List[Int]]):Int = {

    def randomItem(items:Iterable[Int]):Int = {
      val itemsArray = items.toArray
      itemsArray(random.nextInt(itemsArray.length))
    }

    def numOfEdges(graph:Map[Int, List[Int]]):Int =
      graph.mapValues(_.size).foldLeft(0)((a, e) => a + e._2)/2

    if (graph.size <= 2) numOfEdges(graph)
    else {
      val v1 = randomItem(graph.keys)
      val v2 = randomItem(graph(v1))
      val g1 = graph.mapValues(_.map(x => if (x == v2) v1 else x))
      val g2 = g1 + (v1 -> (g1(v1) ++ g1(v2)).filter(_ != v1))
      minCut(g2 - v2)
    }
  }

  def main(args: Array[String]) {
    val g = readFromFile("kargerMinCut.txt")
    println((1 to 100).map(i => minCut(g)).min)
  }

}
