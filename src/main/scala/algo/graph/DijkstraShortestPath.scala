package algo.graph

import algo.struct.BinaryHeap

import scala.language.implicitConversions

/**
  * Created by serg on 8/4/16.
  */
object DijkstraShortestPath {

  class DVertex(val id:Int, var distance:Int = Int.MaxValue) extends Vertex

  implicit def createVertex(id:Int):DVertex = new DVertex(id)

  def shortestPaths(graph: Graph[DVertex], sourceId:Int):Unit = {

    graph.vertex(sourceId).foreach(_.distance = 0)

    implicit val vOrdering: Ordering[DVertex] = Ordering.by[DVertex, Int](_.distance)

    val heap = new BinaryHeap[DVertex]()
    for {
      v <- graph.vertices
    } {
      heap.insert(v)
    }

    def relax(v1: DVertex, v2: DVertex): Unit = {
      val dist = v1.distance + graph.edge(v1, v2).map(_.weight).toVector.min
      if (v2.distance > dist) {
        heap.removeAll(v2)
        v2.distance = dist
        heap.insert(v2)
      }
    }

    while(heap.nonEmpty) {
      val v1 = heap.extractMin()
      for (v2 <- graph.adjList(v1)) {
        relax(v1, v2)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val g = Graph.readWeightedAdjacencyList("dijkstraData.txt")
    shortestPaths(g, 1)
    val expected = List(2599, 2610, 2947, 2052, 2367, 2399, 2029, 2442, 2505, 3068)
    println(List(7,37,59,82,99,115,133,165,188,197).map(id => g.vertex(id).get.distance) == expected)
  }

}
