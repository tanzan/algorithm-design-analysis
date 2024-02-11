import algo.struct.{Referencing, ReferencingBinaryHeap}
import org.scalameter.Bench
import org.scalameter.api.Gen

import scala.collection.mutable
import scala.util.Random

object BinaryHeapBenchmark extends Bench.LocalTime {

  implicit val dummyReferencing: Referencing[Int] = new Referencing[Int] {
    override def update(x: Int, i: Int): Unit = {}

    override def of(x: Int): Int = 0
  }

  private val sizes = Gen.range("size")(10000, 100000, 10000)

  private val ranges = for {
    size <- sizes
  } yield Random.shuffle((0 until size).toVector)

  performance of "BinaryHeap" in {

    measure method "insert" in {
      using(ranges) in {
        r =>
          val heap = new ReferencingBinaryHeap[Int]()
          r.foreach(heap.insert)
      }
    }

    measure method "extractMin" in {
      using(ranges) in {
        r =>
          val heap = new ReferencingBinaryHeap[Int]()
          r.foreach(heap.insert)
          while (heap.nonEmpty) {
            heap.extractMin()
          }
      }
    }

    measure method "remove" in {
      class Item(val value: Int, var index: Int = Referencing.Nil) {
        override def toString: String = s"$value($index)"
        override def equals(obj: scala.Any): Boolean = obj match {
          case i: Item => value == i.value
          case _ => false
        }
        override def hashCode(): Int = value
      }
      implicit val ord: Ordering[Item] = Ordering.by[Item, Int](_.value)
      implicit val ref: Referencing[Item] = new Referencing[Item] {
        override def update(x: Item, i: Int): Unit = x.index = i
        override def of(x: Item): Int = x.index
      }
      using(ranges.map(_.map(new Item(_)))) in {
        r =>
          val heap = new ReferencingBinaryHeap[Item]()
          r.foreach(heap.insert)
          r.foreach(heap.remove)
      }
    }

  }

  performance of "TreeSet" in {
    measure method "add" in {
      using(ranges) in {
        r =>
          val set = mutable.TreeSet[Int]()
          r.foreach(set.add)
      }
    }

    measure method "extractMin" in {
      using(ranges) in {
        r =>
          val set = mutable.TreeSet[Int]()
          r.foreach(set.add)
          while (set.nonEmpty) {
            set.remove(set.min)
          }
      }
    }
  }

  performance of "PriorityQueue" in {
    measure method "enqueue" in {
      using(ranges) in {
        r =>
          val queue = mutable.PriorityQueue[Int]()
          r.foreach(queue.addOne)
      }
    }

    measure method "dequeue" in {
      using(ranges) in {
        r =>
          val queue = mutable.PriorityQueue[Int]()
          r.foreach(queue.addOne)
          while (queue.nonEmpty) {
            queue.dequeue()
          }
      }
    }
  }


}
