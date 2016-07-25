import scala.io.Source
import scala.util.Random

/**
  * Created by serg on 7/22/16.
  */
object KargerMinCut {

  val random = new Random()

  def readFromFile(fileName:String):Map[Int, Set[Int]] = {
    println(Source.fromFile(fileName).getLines().size)
    Source.fromFile(fileName).getLines().map(line => {
      val vertices = line.split("\\s+").map(_.trim.toInt).toList
      vertices.head -> vertices.tail.toSet
    }).toMap
  }

  def minCut(graph:Map[Int, Set[Int]]):Int = {

    def randomItem(items:Iterable[Int]):Int = {
      val itemsArray = items.toArray
      itemsArray(random.nextInt(itemsArray.size))
    }

    def numOfEdges(graph:Map[Int, Set[Int]]):Int =
      graph.mapValues(_.size).foldLeft(0)((a, e) => a + e._2)/2

    if (graph.size <= 2) numOfEdges(graph)
    else {
      val v1 = randomItem(graph.keys)
      val v2 = randomItem(graph(v1))
      println((v1, v2))
      minCut(graph - v2 + (v1 -> (graph(v1).filter(_ != v2) ++ graph(v2))))
    }
  }

  def main(args: Array[String]) {
    val g = readFromFile("kargerMinCut.txt")
    //println(g(52))
    println(g.size)
    println(minCut(g))
  }

}
