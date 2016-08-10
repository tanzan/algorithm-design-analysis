package algo.graph

import scala.collection.{Iterable, Seq, mutable}
import scala.io.Source

/**
  * Created by serg on 06.08.16.
  */
class Graph[V <: Vertex] {
  private val _vertices = mutable.Map[Int, V]()
  private val _adjList = mutable.Map[V, mutable.ArrayBuffer[V]]()
  private val _edges = mutable.Map[(V, V), mutable.ArrayBuffer[Edge[V]]]()

  def addEdge(vertexId1:Int, vertex2Id: Int, weight:Int = 0)(implicit createVertex: Int => V):Unit =  {
    val v1 = _vertices.getOrElseUpdate(vertexId1, createVertex(vertexId1))
    val v2 = _vertices.getOrElseUpdate(vertex2Id, createVertex(vertex2Id))
    _adjList.getOrElseUpdate(v1, mutable.ArrayBuffer[V]()) += v2
    _edges.getOrElseUpdate((v1, v2), mutable.ArrayBuffer[Edge[V]]()) += Edge[V](v1, v2, weight)
  }

  def vertex(id:Int):Option[V] = _vertices.get(id)
  def vertices:Iterable[V] = _vertices.values
  def adjList(vertex: V):Iterable[V] = _adjList.getOrElse(vertex, Seq.empty)
  def edges:Iterable[Edge[V]] = _edges.values.flatten
  def edge(vertex1: V, vertex2: V):Iterable[Edge[V]] = _edges((vertex1, vertex2))

  def invert()(implicit createVertex:Int => V):Graph[V] = {

    val inverted = new Graph[V]

    for(vId <- _vertices.keys) {
      val v1 = _vertices(vId)
      for(v2 <- adjList(v1)) {
        inverted.addEdge(v2.id, v1.id)
      }
    }

    inverted
  }
}

object Graph {

  def readFromFile[V <: Vertex](fileName:String)(implicit createVertex: Int => V):Graph[V] = {

    val graph = new Graph[V]

    Source.fromFile(fileName).getLines().foreach(line => {
      val edge = line.split("\\s+").map(_.trim.toInt).toVector
      graph.addEdge(edge(0), edge(1))
    })

    graph
  }

  def readWeightedFromFile[V <: Vertex](fileName:String)(implicit createVertex: Int => V):Graph[V] = {

    val graph = new Graph[V]

    Source.fromFile(fileName).getLines().foreach(line => {
      val adjList = line.split("\\s+").map(_.trim.split(",").map(_.trim.toInt)).toVector
      for (i <- adjList.indices.tail)
        graph.addEdge(adjList(0)(0), adjList(i)(0), adjList(i)(1))
    })

    graph
  }

}