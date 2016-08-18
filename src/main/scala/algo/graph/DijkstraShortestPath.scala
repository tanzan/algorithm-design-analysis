package algo.graph

import scala.collection.mutable

/**
  * Created by serg on 8/4/16.
  */
object DijkstraShortestPath {

  class DVertex(val id:Int, var distance:Int = Int.MaxValue) extends Vertex

  implicit def createVertex(id:Int):DVertex = new DVertex(id)

  def shortestPaths(graph: Graph[DVertex], sourceId:Int):Unit = {

    def relax(v1:DVertex, v2:DVertex):Unit = {
      val dist = v1.distance + graph.edge(v1, v2).map(_.weight).toVector.min
      if (v2.distance > dist) {
        v2.distance = dist
      }
    }

    graph.vertex(sourceId).foreach(_.distance = 0)

    val heap = mutable.Set[DVertex]()
    heap ++= graph.vertices

    while(heap.nonEmpty) {
      val v1 = heap.minBy(_.distance)
      heap -= v1
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
