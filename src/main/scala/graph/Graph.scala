package graph

import scala.collection.{Iterable, Seq, mutable}
import scala.io.Source

/**
  * Created by serg on 06.08.16.
  */
class Graph[V <: Vertex] {
  private val _vertices = mutable.Map[Int, V]()
  private val _adjList = mutable.Map[V, mutable.ArrayBuffer[V]]()

  def addEdge(vertexId1:Int, vertex2Id: Int)(implicit createVertex: Int => V):Unit =  {
    val v1 = _vertices.getOrElseUpdate(vertexId1, createVertex(vertexId1))
    val v2 = _vertices.getOrElseUpdate(vertex2Id, createVertex(vertex2Id))
    _adjList.getOrElseUpdate(v1, mutable.ArrayBuffer[V]()) += v2
  }

  def vertex(id:Int):Option[V] = _vertices.get(id)
  def vertices:Iterable[V] = _vertices.values
  def adjList(vertex: V):Iterable[V] = _adjList.getOrElse(vertex, Seq.empty)

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

}