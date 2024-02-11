package algo.struct

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class HeapSpec extends AnyFlatSpec with should.Matchers {

  implicit val dummyReferencing: Referencing[Int] = new Referencing[Int] {
    override def update(x: Int, i: Int): Unit = {}
    override def of(x: Int): Int = 0
  }

  "A Heap" should "insert values and provide min" in {
    val heap = new ReferencingBinaryHeap[Int]()
    heap.insert(3)
    heap.min should be (3)
    heap.insert(1)
    heap.min should be (1)
    heap.insert(2)
    heap.min should be (1)

    heap.isEmpty should be (false)
    heap.size should be (3)
  }

  it should "extract min" in {
    val heap = new ReferencingBinaryHeap[Int]()
    heap.insert(3)
    heap.insert(2)
    heap.insert(1)
    heap.insert(2)
    heap.extractMin() should be (1)
    heap.extractMin() should be (2)
    heap.extractMin() should be (2)
    heap.extractMin() should be (3)

    heap.isEmpty should be(true)
    heap.size should be(0)
  }

  it should "remove element" in {

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

    val one = new Item(1)
    val two = new Item(2)
    val anotherTwo = new Item(2)
    val three = new Item(3)

    val heap = new ReferencingBinaryHeap[Item]()
    heap.insert(three)
    heap.insert(two)
    heap.insert(one)
    heap.insert(anotherTwo)

    heap.remove(one)
    heap.min should be (two)

    heap.remove(two)
    heap.min should be(anotherTwo)

    heap.remove(anotherTwo)
    heap.min should be (three)

    heap.remove(three)

    heap.isEmpty should be (true)

  }
}