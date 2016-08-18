package algo.greedy

import algo.graph.{Graph, Vertex}

import scala.collection.mutable

/**
  * Created by serg on 18.08.16.
  */
object SingleLinkClustering {

  class CVertex(val id:Int, var leader:Int, val followers:mutable.Set[CVertex] = mutable.Set.empty) extends Vertex

  implicit def createVertex(id:Int):CVertex =  {
    val v = new CVertex(id, id)
    v.followers += v
    v
  }

  def kClustering(graph: Graph[CVertex], k:Int):(Int, Set[CVertex]) = {
    var queue = graph.edges.toVector.sortWith(_.weight < _.weight)
    val clusters = mutable.Set.empty[CVertex]
    clusters ++= graph.vertices
    var maxSpacing = 0
    while(clusters.size > k) {
      val edge = queue.head
      queue = queue.tail
      if (edge.v1.leader != edge.v2.leader) {
        maxSpacing = edge.weight
        val leader1 = graph.vertex(edge.v1.leader).get
        val leader2 = graph.vertex(edge.v2.leader).get
        if (leader1.followers.size < leader2.followers.size) {
          leader1.followers.foreach(v => v.leader = leader2.id)
          leader1.leader = leader2.id
          leader2.followers ++= leader1.followers
          leader1.followers.clear()
          clusters -= leader1
        } else {
          leader2.followers.foreach(v => v.leader = leader1.id)
          leader2.leader = leader1.id
          leader1.followers ++= leader2.followers
          leader2.followers.clear()
          clusters -= leader2
        }
      }
    }
    (maxSpacing, clusters.toSet)
  }

  def main(args: Array[String]): Unit = {
    val g = Graph.read("clustering1.txt")
    val res = kClustering(g, 5)
    println(g.vertices.size)
    println(res._1)
    println(res._2.map(_.followers.map(_.id)).toVector)
  }

}
