package algo.dynprog

import scala.io.Source

/**
  * Created by serg on 9/5/16.
  */
object TravelingSalesman {

  case class Location(id:Int, x:Double, y:Double) {
    def distance(other:Location):Double =
      Math.sqrt(Math.pow(x - other.x, 2) + Math.pow(y - other.y, 2))
  }

  case class Solution(locations:Set[Location], dist:Double, last:Location) {
    def +(location: Location):Solution =
      Solution(locations + location, dist + (if (locations.isEmpty) 0.0 else locations.map(_.distance(location)).min), location)
  }

  def tsp(locations:Seq[Location], start:Int):Double = {
    locations.toStream.foldLeft(Set(Solution(Set(locations(start)), 0, locations(start))).toStream) ((a, e) => a.flatMap(s => Set(s) ++ Set(s + e)))
      .filter(loc => loc.locations.size  == locations.size - 1 && loc.locations.exists(_.id == start))
      .map(solution => solution.dist + solution.last.distance(locations(start))).min
  }

  def readLocations(fileName:String):Seq[Location] =
    (for(line <- Source.fromFile(fileName).getLines().drop(1).zipWithIndex) yield {
      val loc = line._1.split("\\s")
      Location(line._2, loc(0).toDouble, loc(1).toDouble)
    }).toVector


  def main(args: Array[String]): Unit = {
    val locations = readLocations("tsp.txt")
    println(locations)
    println(locations.size)
    println(tsp(locations, 1))
  }

}
