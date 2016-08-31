package algo.dynprog

import algo.graph.{Graph, Vertex}

/**
  * Created by serg on 31.08.16.
  */
object AllPairsShortestPaths {

  class IDVertex(val id:Int) extends Vertex

  implicit def createIDVertex(id:Int):IDVertex = new IDVertex(id)

  def floydWarshall(graph:Graph[IDVertex]):Array[Array[Array[Int]]] = {
    val vertices = graph.vertices.toSeq
    val solutions = Array.ofDim[Int](vertices.size, vertices.size, vertices.size + 1)

    for(i <- vertices.indices){
      for(j <- vertices.indices) {
        if (i == j) solutions(i)(j)(0) = 0
        else
          solutions(i)(j)(0) = graph.edge(vertices(i), vertices(j))
            .headOption.map(_.weight).getOrElse(Int.MaxValue)
      }
    }

    for(k <- 1 to vertices.size) {
      for(i <- vertices.indices) {
        for(j <- vertices.indices) {
          solutions(i)(j)(k) = Math.min(
            solutions(i)(j)(k - 1),
            solutions(i)(k - 1)(k - 1) + solutions(k - 1)(j)(k - 1)
          )
        }
      }
    }

    solutions
  }

  def main(args: Array[String]): Unit = {
    val g1 = Graph.read("apsp-g1.txt")
    println(g1.vertices.size)
    //println(floydWarshall(g1))
  }

}
