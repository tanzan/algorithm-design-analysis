package algo.greedy

import algo.graph.{Edge, Graph, Vertex}

import scala.collection.mutable
import scala.io.Source

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
      if (edge.v1.leader != edge.v2.leader) {
        if (edge.v1.leader.followers.size < edge.v2.leader.followers.size) {
          clusters -= edge.v1.leader
          edge.v2.leader.merge(edge.v1.leader)
        } else {
          clusters -= edge.v2.leader
          edge.v1.leader.merge(edge.v2.leader)
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


  final val LabelSize = 24

  def readHypercube(fileName:String):Iterable[CVertex] =
    (for(line <- Source.fromFile(fileName).getLines().drop(1))
      yield createVertex(Integer.parseInt(line.replace(" ", ""), 2))).toIterable


  def numOfHypercubeClusters(vertices:Iterable[CVertex], minSpacing:Int):Int = {

    val vertexMap:Map[Int, CVertex] = vertices.map(v => v.id -> v).toMap

    def bitsToInt(bits:Vector[Int]):Int =
      bits.zipWithIndex.foldLeft(0)((n, b) => n | (b._1 << b._2))

    def edgesWithDistance(dist:Int):Iterable[Edge[CVertex]] = {
      val diffs = (Vector.fill(dist)(1) ++ Vector.fill(LabelSize - dist)(0))
        .permutations.map(x => bitsToInt(x)).toIterable
      for(d <- diffs; v <- vertices if vertexMap.contains(v.id ^ d))
        yield Edge[CVertex](v, vertexMap(v.id ^ d), dist)
    }

    val clusters = mutable.Set.empty[CVertex]
    clusters ++= vertices

    for(spacing <- 0 until  minSpacing) {
      for(edge <- edgesWithDistance(spacing)){
        if (edge.v1.leader != edge.v2.leader) {
          if (edge.v1.leader.followers.size < edge.v2.leader.followers.size) {
            clusters -= edge.v1.leader
            edge.v2.leader.merge(edge.v1.leader)
          } else {
            clusters -= edge.v2.leader
            edge.v1.leader.merge(edge.v2.leader)
          }
        }
      }
    }

    clusters.size
  }

  def main(args: Array[String]): Unit = {
    val g = Graph.read("clustering1.txt")
    val res = kClustering(g, 4)
    println(res._1 == 106)

    val hypercube = readHypercube("clustering_big.txt")
    println(numOfHypercubeClusters(hypercube, 3) == 6118)
  }

}
