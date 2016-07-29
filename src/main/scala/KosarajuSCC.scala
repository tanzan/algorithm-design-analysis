import scala.annotation.tailrec
import scala.io.Source
import scala.collection._

/**
  * Created by serg on 7/29/16.
  */
object KosarajuSCC {

  class Vertex(val id:Int, var visited:Boolean = false,
               var finishTime:Int = 0, var leader:Option[Vertex] = None)

  class Graph {
    private val _vertices = mutable.Map[Int, Vertex]()
    private val _adjList = mutable.Map[Vertex, mutable.ArrayBuffer[Vertex]]()

    def addEdge(vertexId1:Int, vertex2Id: Int):Unit =  {
      val v1 = _vertices.getOrElseUpdate(vertexId1, new Vertex(vertexId1))
      val v2 = _vertices.getOrElseUpdate(vertex2Id, new Vertex(vertex2Id))
      _adjList.getOrElseUpdate(v1, mutable.ArrayBuffer[Vertex]()) += v2
    }

    def vertices:Map[Int, Vertex] = _vertices
    def adjList(vertex: Vertex):Seq[Vertex] = _adjList.getOrElse(vertex, Seq.empty)

    def reverse():Graph = {

      val reversed = new Graph

      for(vId <- vertices.keys) {
        val v1 = vertices(vId)
        for(v2 <- adjList(v1)) {
          reversed.addEdge(v1.id, v2.id)
        }
      }

      reversed
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

  def dfs(graph: Graph, start:Vertex): Unit = {
    start.visited = true
    for (v <- graph.adjList(start) if !v.visited) {
      if (!v.visited) dfs(graph, v)
    }
  }


  def main(args: Array[String]): Unit = {
    val g = readFromFile("SCC.txt")
    println(g.vertices.size)
    g.vertices.values.foreach(v => if (!v.visited) dfs(g, v))
    println(g.vertices.values.forall(_.visited))
  }

}
