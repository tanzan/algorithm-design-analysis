package algo.greedy

import algo.graph.{Graph, Vertex, Edge}

import scala.collection.mutable

/**
  * Created by serg on 18.08.16.
  */
object SingleLinkClustering {

  class CVertex(val id:Int, var leader:CVertex, val followers:mutable.Set[CVertex]) extends Vertex {
    def merge(that: CVertex):Unit = {
      that.followers.foreach(v => v.leader = this)
      followers ++= that.followers
      that.followers.clear()
    }
  }

  implicit def createVertex(id:Int):CVertex = {
    val v = new CVertex(id, null, mutable.Set.empty[CVertex])
    v.leader = v
    v.followers += v
    v
  }

  def kClustering(graph: Graph[CVertex], k:Int):(Int, Set[CVertex]) = {
    var queue = graph.edges.toVector.sortWith(_.weight < _.weight)

    def extractMin():Edge[CVertex] = {
      val edge = queue.head
      queue = queue.tail
      edge
    }

    val clusters = mutable.Set.empty[CVertex]
    clusters ++= graph.vertices

    while(clusters.size > k) {
      val edge = extractMin()
      if (edge.v1.leader.id != edge.v2.leader.id) {
        val leader1 = edge.v1.leader
        val leader2 = edge.v2.leader
        if (leader1.followers.size < leader2.followers.size) {
          leader2.merge(leader1)
          clusters -= leader1
        } else {
          leader1.merge(leader2)
          clusters -= leader2
        }
      }
    }

    var maxSpacing = Int.MaxValue

    while (queue.nonEmpty && maxSpacing == Int.MaxValue) {
      val edge = extractMin()
      if (edge.v1.leader != edge.v2.leader) {
        maxSpacing = edge.weight
      }
    }
    (maxSpacing, clusters.toSet)

  }

  def main(args: Array[String]): Unit = {
    val g = Graph.read("clustering1.txt")
    val res = kClustering(g, 4)
    println(res._1 == 106)
  }

}
