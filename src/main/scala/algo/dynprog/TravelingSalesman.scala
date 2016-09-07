package algo.dynprog

import scala.collection.mutable
import scala.io.Source

/**
  * Created by serg on 9/5/16.
  */
object TravelingSalesman {

  case class Location(id:Int, x:Double, y:Double) {
    def distance(other:Location):Double =
      Math.sqrt(Math.pow(x - other.x, 2) + Math.pow(y - other.y, 2))
  }

  def tsp(locations:Seq[Location], start:Int):Double = {

    def keyOf(set:Iterable[Location], k:Int): Long =
      (set.foldLeft(0L)((key,loc) => key | (1 << loc.id)) << 32) | k

    var current = mutable.Map[Long, Double]()
    current += keyOf(Set(locations(start)), start) -> 0.0
    var former:mutable.Map[Long, Double] = null
    for(m <- 2 to locations.size) {
      println(m)
      former = current
      current = mutable.Map[Long,  Double]()
      for (s <- locations.combinations(m).map(_.toSet) if s.exists(_.id == start)){
        val sId = keyOf(s, 0)
        for(j <- s if j != locations(start)) {
          var dist = Double.PositiveInfinity
          for(k <- s if k != j) {
            dist = Math.min(dist, former.getOrElse((sId ^ ((1L << j.id) << 32)) | k.id, Double.PositiveInfinity) + k.distance(j))
          }
          current += (sId | j.id) -> dist
        }
      }
    }
    current.map(solution => locations(solution._1.toInt).distance(locations(start)) + solution._2).min
  }

  def readLocations(fileName:String):Seq[Location] =
    (for(line <- Source.fromFile(fileName).getLines().drop(1).zipWithIndex) yield {
      val loc = line._1.split("\\s")
      Location(line._2, loc(0).toDouble, loc(1).toDouble)
    }).toVector


  def main(args: Array[String]): Unit = {
    val locations = readLocations("tsp.txt")
    println(tsp(locations, 0))
  }

}
