package algo.struct

import scala.annotation.tailrec
import scala.collection.mutable

class BinaryHeap[A: Ordering] {

  private val storage = mutable.ArrayBuffer[A]()
  private val index = mutable.HashMap[A, mutable.HashSet[Int]]()

  def size: Int = storage.size

  def isEmpty: Boolean = storage.isEmpty

  def nonEmpty: Boolean = storage.nonEmpty

  private def parent(i: Int): Int = (i - 1) / 2

  @inline
  private def left(i: Int): Int = 2 * i + 1

  @inline
  private def right(i: Int): Int = 2 * i + 2

  private def swap(i: Int, j: Int): Unit = {
    index.getOrElseUpdate(storage(i), mutable.HashSet()) += j
    index(storage(i)) -= i

    index.getOrElseUpdate(storage(j), mutable.HashSet()) += i
    index(storage(j)) -= j

    val tmp = storage(j)
    storage(j) = storage(i)
    storage(i) = tmp
  }

  @inline
  private def lt(a: A, b: A): Boolean = implicitly[Ordering[A]].lt(a, b)

  @tailrec
  private def heapifyDown(i: Int, n: Int): Unit = {
    val l = left(i)
    val r = right(i)
    if (l < n) {
      val j = if (r < n && lt(storage(r), storage(l))) {
        right(i)
      } else l
      if (lt(storage(j), storage(i))) {
        swap(i, j)
        heapifyDown(j, n)
      }
    }
  }

  @tailrec
  private def heapifyUp(i: Int): Unit = {
    val j = parent(i)
    if (i > 0 && lt(storage(i), storage(j))) {
      swap(i, j)
      heapifyUp(j)
    }
  }

  def insert(x: A): Unit = {
    storage.addOne(x)
    index.getOrElseUpdate(x, mutable.HashSet()) += storage.size - 1
    heapifyUp(storage.size - 1)
  }

  def extractMin(): A = {
    val result = storage.head
    swap(0, storage.size - 1)
    heapifyDown(0, storage.size - 1)
    index.remove(storage.last)
    storage.remove(storage.size - 1)
    result
  }

  def min: A = storage.head

  private def remove(i: Int): Unit = {
    swap(i, storage.size - 1)
    if (lt(storage(i), storage(parent(i)))) {
      heapifyUp(i)
    } else {
      heapifyDown(i, storage.size - 1)
    }
    index(storage.last) -= storage.size - 1
    storage.remove(storage.size - 1)
  }

  def removeAll(x: A): Unit = {
    @tailrec
    def rm(is: mutable.Set[Int]): Unit = {
      if (is.nonEmpty) {
        remove(is.head)
        rm(is)
      }
    }
    if (index.contains(x)) {
      rm(index(x))
      index.remove(x)
    }
  }
}
