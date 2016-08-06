package graph

import scala.io.Source

/**
  * Created by serg on 7/29/16.
  */
object KosarajuSCC {

  import Graph._

  class KVertex(val id:Int, var visited:Boolean = false,
                var finishTime:Int = 0, var leader:Option[KVertex] = None) extends Vertex

  implicit def createVertex(id:Int):KVertex = new KVertex(id)

  def dfs(graph: Graph[KVertex], start:KVertex)(before: KVertex => Unit)(after: KVertex => Unit): Unit = {
    start.visited = true
    before(start)
    for (v <- graph.adjList(start) if !v.visited) {
      dfs(graph, v)(before)(after)
    }
    after(start)
  }

  def scc(graph: Graph[KVertex]):Unit = {

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
    val g = readFromFile[KVertex]("SCC.txt")
    scc(g)
    println(g.vertices.groupBy(_.leader).mapValues(_.size).values.toVector.sortWith(_ > _).take(5))
  }

}
