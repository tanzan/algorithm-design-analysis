package graph

import scala.collection._
import scala.io.Source

/**
  * Created by serg on 7/29/16.
  */
object KosarajuSCC {

  trait Vertex {
    def id:Int
  }

  class KVertex(val id:Int, var visited:Boolean = false,
                var finishTime:Int = 0, var leader:Option[KVertex] = None)

  class Graph {
    private val _vertices = mutable.Map[Int, KVertex]()
    private val _adjList = mutable.Map[KVertex, mutable.ArrayBuffer[KVertex]]()

    def addEdge(vertexId1:Int, vertex2Id: Int):Unit =  {
      val v1 = _vertices.getOrElseUpdate(vertexId1, new KVertex(vertexId1))
      val v2 = _vertices.getOrElseUpdate(vertex2Id, new KVertex(vertex2Id))
      _adjList.getOrElseUpdate(v1, mutable.ArrayBuffer[KVertex]()) += v2
    }

    def vertex(id:Int):Option[KVertex] = _vertices.get(id)
    def vertices:Iterable[KVertex] = _vertices.values
    def adjList(vertex: KVertex):Iterable[KVertex] = _adjList.getOrElse(vertex, Seq.empty)

    def invert():Graph = {

      val inverted = new Graph

      for(vId <- _vertices.keys) {
        val v1 = _vertices(vId)
        for(v2 <- adjList(v1)) {
          inverted.addEdge(v2.id, v1.id)
        }
      }

      inverted
    }
  }

  def readFromFile(fileName:String):Graph = {

    val graph = new Graph

    Source.fromFile(fileName).getLines().foreach(line => {
      val edge = line.split("\\s+").map(_.trim.toInt).toVector
      graph.addEdge(edge(0), edge(1))
    })

    graph
  }

  def dfs(graph: Graph, start:KVertex)(before: KVertex => Unit)(after: KVertex => Unit): Unit = {
    start.visited = true
    before(start)
    for (v <- graph.adjList(start) if !v.visited) {
      dfs(graph, v)(before)(after)
    }
    after(start)
  }

  def scc(graph: Graph):Unit = {

    val inverted = graph.invert()
    var finishTime = 0
    var leader:Option[KVertex] = None

    for(s <- inverted.vertices if !s.visited) {
      dfs(inverted, s) (v => ()){ v =>
        v.finishTime = finishTime
        finishTime += 1
      }
    }

    for(i <- inverted.vertices.toVector.sortWith(_.finishTime > _.finishTime)) {
      val s = graph.vertex(i.id)
      if (!s.forall(_.visited)) {
        leader = s
        dfs(graph, s.get)(v => v.leader = leader)(v => ())
      }
    }

  }


  def main(args: Array[String]): Unit = {
    val g = readFromFile("SCC.txt")
    scc(g)
    println(g.vertices.groupBy(_.leader).mapValues(_.size).values.toVector.sortWith(_ > _).take(5))
  }

}
