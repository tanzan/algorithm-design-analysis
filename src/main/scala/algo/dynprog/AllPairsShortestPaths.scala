package algo.dynprog

import algo.graph.{Graph, Vertex}

/**
  * Created by serg on 31.08.16.
  */
object AllPairsShortestPaths {

  case class IDVertex(id:Int) extends Vertex

  implicit def createIDVertex(id:Int):IDVertex = IDVertex(id)


  class FWSolution(size: Int) {

    private var k = 0
    private var prev:Array[Array[Int]] = _
    private var current:Array[Array[Int]] = Array.ofDim(size, size)

    def nonComplete():Boolean = k < size

    def isComplete():Boolean = !nonComplete()

    def next():Unit = {
      k += 1
      prev = current
      current = Array.ofDim[Int](size, size)
    }

    def apply(i:Int, j:Int, k:Int):Int = {
      if (k > this.k || k < this.k - 1) throw new ArrayIndexOutOfBoundsException
      if (k == this.k - 1) prev(i)(j)
      else current(i)(j)
    }

    def update(i:Int, j:Int, k:Int, value:Int):Unit = {
      if (k != this.k) throw new IllegalArgumentException
      current(i)(j) = value
    }

    def minPath:Option[(Int, Int, Int)] = {
      var ms:Option[(Int, Int, Int)] = None
      for(i <- 0 until size) {
        for(j <- 0 until size) {
          if (i == j && current(i)(j) < 0) {
            return None
          }
          if (ms.isEmpty || ms.forall(_._3 > current(i)(j))) ms = Some((i, j, current(i)(j)))
        }
      }
      ms
    }

  }

  final val PlusInfinity = Int.MaxValue

  def floydWarshall(graph:Graph[IDVertex]):FWSolution = {

    val vertices = graph.vertices.toSeq
    val solution = new FWSolution(vertices.size)

    for(i <- vertices.indices){
      for(j <- vertices.indices) {
        if (i == j) solution(i, j, 0) = 0
        else
          solution(i, j, 0) = graph.edge(vertices(i), vertices(j))
            .headOption.map(_.weight).getOrElse(PlusInfinity)
      }
    }

    def add(x:Int, y:Int) =
      if (x == PlusInfinity || y == PlusInfinity) PlusInfinity
      else x + y

    for(k <- 1 to vertices.size) {
      solution.next()
      for(i <- vertices.indices) {
        for(j <- vertices.indices) {
          solution(i, j, k) = Math.min(
            solution(i, j, k - 1),
            add(solution(i, k - 1, k - 1), solution(k - 1, j, k - 1))
          )
        }
      }
    }

    solution
  }

  def main(args: Array[String]): Unit = {
    val s1 = floydWarshall(Graph.read("apsp-g1.txt", skip = 1)).minPath
    println(s1.isEmpty)
    val s2 = floydWarshall(Graph.read("apsp-g2.txt", skip = 1)).minPath
    println(s2.isEmpty)
    val s3 = floydWarshall(Graph.read("apsp-g3.txt", skip = 1)).minPath
    println(s3.forall(_._3 == -19))
  }

}
