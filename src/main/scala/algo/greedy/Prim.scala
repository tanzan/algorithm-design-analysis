package algo.greedy

import algo.graph.{Edge, Graph, Vertex}

import scala.collection.mutable
import scala.language.implicitConversions


object Prim {

  class PVertex(val id: Int, var weight: Int = Int.MaxValue) extends Vertex {
    override def toString: String = s"$id($weight)"
    override def equals(obj: scala.Any): Boolean = obj match {
      case v: PVertex => id == v.id
      case _ => false
    }
    override def hashCode(): Int = id
  }

  implicit def createVertex(id: Int): PVertex = new PVertex(id)


  def minimumSpanningTree(graph: Graph[PVertex]): Set[Edge[PVertex]] = {

    implicit val vOrder: Ordering[PVertex] = Ordering.by((v: PVertex) => v.weight)

    val heap = mutable.HashSet[PVertex]()

    def extractMin(): PVertex = {
      val v = heap.min
      heap.remove(v)
      v
    }

    val winner = mutable.HashMap.empty[PVertex, Edge[PVertex]]

    val start = graph.vertices.head
    val mstVertices = mutable.Set.empty[PVertex]
    mstVertices += start

    var z = 0

    for {
      v <- graph.adjList(start)
      edge <- graph.edge(start, v)
      if v.weight > edge.weight
    } {
      heap.remove(v)
      v.weight = edge.weight
      winner.put(v, edge)
      heap.add(v)
    }

    val mst = mutable.Set.empty[Edge[PVertex]]

    while (heap.nonEmpty) {
      val v = extractMin()
      mstVertices += v
      mst += winner(v)
      for {
        w <- graph.adjList(v)
        if !mstVertices.contains(w)
        edge <- graph.edge(v, w)
        if w.weight > edge.weight
      } {
        heap.remove(w)
        w.weight = edge.weight
        heap.add(w)
        winner.put(w, edge)
      }
    }

    mst.toSet
  }

  private def test(caseName: String, expectedValue: Int): Unit = {
    val result = minimumSpanningTree(Graph.read(s"prim_$caseName.txt", 1, directed = false)).toSeq.map(_.weight).sum
    assert(result == expectedValue, s"$caseName: $result != $expectedValue")
  }

  def main(args: Array[String]): Unit = {
    test("1_10", -7430)
    test("4_10", -45855)
    test("5_20", -10519)
    test("12_40", -41633)
    test("15_80", -194537)
    test("18_100", -184735)
    test("23_200", -246552)
    test("24_200", -513349)
    test("25_400", -1120098)
  }
}



