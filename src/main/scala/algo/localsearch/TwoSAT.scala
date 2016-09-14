package algo.localsearch

import scala.io.Source
import scala.util.Random

/**
  * Created by serg on 9/14/16.
  */
object TwoSAT {

  val random = Random

  case class Clause(x:Int, y:Int) {
    def randomFlip():Clause =
      if (random.nextBoolean()) copy(x = -x)
      else copy(y = -y)
  }

  def papadimitrou(clauses:Seq[Clause]):Boolean = {

    for(i <- 1 to (Math.log(clauses.size)/Math.log(2)).toInt){
      var assignment = clauses.map(_.randomFlip().randomFlip())
      for(j <- 1 to 2*clauses.size*clauses.size) {
        if (clauses.zip(assignment).forall(pair => pair._1 == pair._2)) return true
        val index = random.nextInt(assignment.size)
        val clause = assignment(index)
        assignment = (assignment.take(index) :+ clause.randomFlip()) ++ assignment.drop(index + 1)
      }
    }

    false
  }

  def readClauses(fileName:String):Seq[Clause] =
    Source.fromFile(fileName).getLines().drop(1).map{ line =>
      val clause = line.split("\\s+")
      Clause(clause(0).toInt, clause(1).toInt)
    }.toSeq


  def main(args: Array[String]): Unit = {
    val cs1 = readClauses("2sat1.txt")
    println(papadimitrou(cs1))

    val cs2 = readClauses("2sat2.txt")
    println(papadimitrou(cs2))

    val cs3 = readClauses("2sat3.txt")
    println(papadimitrou(cs3))

    val cs4 = readClauses("2sat4.txt")
    println(papadimitrou(cs4))

    //val cs5 = readClauses("2sat5.txt")
    //println(papadimitrou(cs5))

    //val cs6 = readClauses("2sat6.txt")
    //println(papadimitrou(cs6))
  }

}
