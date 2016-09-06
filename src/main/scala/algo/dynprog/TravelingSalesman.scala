package algo.dynprog

import scala.io.Source

/**
  * Created by serg on 9/5/16.
  */
object TravelingSalesman {

  case class Location(id:Int, x:Double, y:Double)

  case class Solution(locations:Set[Location], dist:Double)


  def tsp(locations:Seq[Location], start:Int):Double = {

    def solutions(locations: Set[Location]): Stream[Solution] = ???
      //locations.toStream.foldLeft(Set(Solution(Set.empty, Double.PositiveInfinity)).toStream) ((a, e) => a.flatMap(s => Set(s) ++ Set(s + e)))

    solutions(locations.toSet)
      .filter(loc => loc.locations.size  == locations.size - 1 && loc.locations.exists(_.id == start))
      .minBy(_.dist).dist
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
  }

}
