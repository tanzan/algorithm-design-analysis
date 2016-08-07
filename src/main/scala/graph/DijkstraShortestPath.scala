package graph

import scala.collection.mutable

/**
  * Created by serg on 8/4/16.
  */
object DijkstraShortestPath {

  class DVertex(val id:Int, var distance:Int = Int.MaxValue) extends Vertex

  implicit def createVertex(id:Int):DVertex = new DVertex(id)

  def shortestPaths(graph: Graph[DVertex], sourceId:Int):Unit = {

    def relax(v1:DVertex, v2:DVertex):Boolean = {
      val dist = v1.distance + graph.edge(v1, v2).map(_.weight).toVector.min
      if (v2.distance > dist) {
        v2.distance = dist
        true
      }
      else false
    }

    graph.vertex(sourceId).foreach(_.distance = 0)

    val scanned = mutable.Set[DVertex]()
    scanned += graph.vertex(sourceId).get
    val size = graph.vertices.size

    while(scanned.size != size) {
      val s = mutable.Set[DVertex]()
      for(v1 <- scanned) {
        for (v2 <- graph.adjList(v1)) {
          if (!scanned.contains(v2)) {
            relax(v1, v2)
          }
          s += v2
        }
      }
      scanned ++= s
    }

  }

  def main(args: Array[String]): Unit = {
    val g = Graph.readWeightedFromFile("dijkstraData.txt")
    shortestPaths(g, 1)
    println(g.vertices.map(_.distance).toVector.sorted.take(11))
  }

}
