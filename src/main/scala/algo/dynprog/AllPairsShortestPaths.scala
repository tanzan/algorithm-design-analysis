package algo.dynprog

import algo.graph.{Graph, Vertex}

/**
  * Created by serg on 31.08.16.
  */
object AllPairsShortestPaths {

  class IDVertex(val id:Int) extends Vertex

  implicit def createIDVertex(id:Int):IDVertex = new IDVertex(id)


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

    def sizes:Array[Array[Int]] = current

    def maxSize:Option[(Int, Int, Int)] = {
      var ms:Option[(Int, Int, Int)] = None
      for(i <- 0 until size) {
        for(j <- 0 until size) {
          if (i == j && current(i)(j) < 0) return None
          if (ms.isEmpty || ms.forall(_._3 < current(i)(j))) ms = Some((i, j, current(i)(j)))
        }
      }
      ms
    }

  }

  def floydWarshall(graph:Graph[IDVertex]):FWSolution = {

    val vertices = graph.vertices.toSeq
    val solution = new FWSolution(vertices.size)

    for(i <- vertices.indices){
      for(j <- vertices.indices) {
        if (i == j) solution(i, j, 0) = 0
        else
          solution(i, j, 0) = graph.edge(vertices(i), vertices(j))
            .headOption.map(_.weight).getOrElse(Int.MaxValue)
      }
    }

    for(k <- 1 to vertices.size) {
      solution.next()
      for(i <- vertices.indices) {
        for(j <- vertices.indices) {
          solution(i, j, k) = Math.min(
            solution(i, j, k - 1),
            solution(i, k - 1, k - 1) + solution(k - 1, j, k - 1)
          )
        }
      }
    }

    solution
  }

  def main(args: Array[String]): Unit = {
    val s1 = floydWarshall(Graph.read("apsp-g1.txt", skip = 1)).maxSize
    println(s1)
    val s2 = floydWarshall(Graph.read("apsp-g2.txt", skip = 1)).maxSize
    println(s2)
    val s3 = floydWarshall(Graph.read("apsp-g3.txt", skip = 1)).maxSize
    println(s3)
  }

}
