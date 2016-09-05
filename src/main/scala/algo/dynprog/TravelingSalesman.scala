package algo.dynprog

import scala.io.Source

/**
  * Created by serg on 9/5/16.
  */
object TravelingSalesman {

  case class Location(x:Double, y:Double)


  def tsp(locations:Seq[Location]):Double = {
    ???
  }

  def readLocations(fileName:String):Seq[Location] =
    (for(line <- Source.fromFile(fileName).getLines().drop(1)) yield {
      val loc = line.split("\\s")
      Location(loc(0).toDouble, loc(1).toDouble)
    }).toVector


  def main(args: Array[String]): Unit = {
    val locations = readLocations("tsp.txt")
    println(locations)
    println(locations.size)
  }

}
