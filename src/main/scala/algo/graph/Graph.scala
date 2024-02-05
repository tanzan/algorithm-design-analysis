package algo.graph

import scala.collection.{Iterable, Seq, mutable}
import scala.io.Source

/**
 * Created by serg on 06.08.16.
 */
class Graph[V <: Vertex](directed: Boolean = true) {

  private val _vertices = mutable.Map[Int, V]()

  private val _adjList = mutable.Map[V, mutable.Set[V]]()

  private val _edges = mutable.LinkedHashMap[(V, V), mutable.Set[Edge[V]]]()

  def addEdge(vertexId1: Int, vertex2Id: Int, weight: Int = 0)(implicit createVertex: Int => V): Unit = {
    val v1 = _vertices.getOrElseUpdate(vertexId1, createVertex(vertexId1))
    val v2 = _vertices.getOrElseUpdate(vertex2Id, createVertex(vertex2Id))
    _adjList.getOrElseUpdate(v1, mutable.Set[V]()) += v2
    if (!directed) {
      _adjList.getOrElseUpdate(v2, mutable.Set[V]()) += v1
    }
    _edges.getOrElseUpdate((v1, v2), mutable.Set[Edge[V]]()) += Edge[V](v1, v2, weight)
  }

  def vertex(id: Int): Option[V] = _vertices.get(id)

  def vertices: Set[V] = _vertices.values.toSet

  def adjList(vertex: V): Set[V] = _adjList.getOrElse(vertex, mutable.Set.empty).toSet

  def edges: Set[Edge[V]] = _edges.values.flatten.toSet

  def edge(vertex1: V, vertex2: V): Set[Edge[V]] =
    (_edges.getOrElse((vertex1, vertex2), mutable.Set.empty) ++ _edges.getOrElse((vertex2, vertex1), mutable.Set.empty)).toSet


  def invert()(implicit createVertex: Int => V): Graph[V] = {

    val inverted = new Graph[V](this.directed)

    for (vId <- _vertices.keys) {
      val v1 = _vertices(vId)
      for (v2 <- adjList(v1)) {
        inverted.addEdge(v2.id, v1.id)
      }
    }

    inverted
  }
}

object Graph {

  def read[V <: Vertex](fileName: String, skip: Int = 0, directed: Boolean = true)(implicit createVertex: Int => V): Graph[V] = {

    val graph = new Graph[V](directed)

    Source.fromFile(fileName).getLines().drop(skip).foreach(line => {
      val edge = line.split("\\s+").map(_.trim.toInt).toVector
      if (edge.length > 1) {
        graph.addEdge(edge(0), edge(1), if (edge.length <= 2) 0 else edge(2))
      }
    })

    graph
  }


  def readWeightedAdjacencyList[V <: Vertex](fileName: String)(implicit createVertex: Int => V): Graph[V] = {

    val graph = new Graph[V]

    Source.fromFile(fileName).getLines().foreach(line => {
      val adjList = line.split("\\s+").map(_.trim.split(",").map(_.trim.toInt)).toVector
      for (i <- adjList.indices.tail)
        graph.addEdge(adjList(0)(0), adjList(i)(0), adjList(i)(1))
    })

    graph
  }

}