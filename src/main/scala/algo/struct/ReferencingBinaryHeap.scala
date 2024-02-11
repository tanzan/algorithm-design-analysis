package algo.struct

trait Referencing[A] {
  def update(a: A, pointer: Int): Unit
  def of(x: A): Int
}

object Referencing {
  val Nil: Int = 0
}

final class ReferencingBinaryHeap[A](implicit ord: Ordering[A], referencing: Referencing[A]) {

  private val StorageInitialSize = 16
  private var storage = new Array[AnyRef](StorageInitialSize)
  private var _size = 1

  def size: Int = _size - 1

  def isEmpty: Boolean = _size <=1

  def nonEmpty: Boolean = !isEmpty

  private def asA(i: Int): A = storage(i).asInstanceOf[A]

  private def swap(i: Int, j: Int): Unit = {
    referencing.update(asA(i), j)
    referencing.update(asA(j), i)
    val tmp = storage(j)
    storage(j) = storage(i)
    storage(i) = tmp
  }

  private def heapifyDown(m: Int, n: Int): Unit = {
    var k = m
    while (2 * k < n) {
      var j = 2 * k
      if ((j + 1) < n && ord.lt(asA(j + 1), asA(j))) {
        j += 1
      }
      if (ord.gteq(asA(j), asA(k))) {
        return // heap property is restored
      } else {
        swap(k, j)
        k = j
      }
    }
  }

  private def heapifyUp(m: Int): Unit = {
    var k = m
    while (k > 1 && ord.lt(asA(k), asA(k / 2))) {
      swap(k, k / 2)
      k = k / 2
    }
  }

  def insert(x: A): Unit = {
    if (_size == storage.length) {
      val newStorage = new Array[AnyRef](storage.length * 2)
      Array.copy(storage, 0, newStorage, 0, storage.length)
      storage = newStorage
    }
    storage(_size) = x.asInstanceOf[AnyRef]
    referencing.update(x, _size)
    heapifyUp(_size)
    _size += 1
  }

  private def removeLast(): Unit = {
    if (isEmpty) {
      throw new IndexOutOfBoundsException("The heap is empty")
    }
    referencing.update(asA(_size - 1), -1)
    storage(_size - 1) = null
    _size -= 1
    if (_size == storage.length / 2) {
      val newStorage = new Array[AnyRef](storage.length / 2)
      Array.copy(storage, 0, newStorage, 0, _size)
      storage = newStorage
    }
  }

  def extractMin(): A = {
    if (isEmpty) {
      throw new IndexOutOfBoundsException("The heap is empty")
    }
    val result = asA(1)
    swap(1, _size - 1)
    heapifyDown(1, _size - 1)
    removeLast()
    result
  }

  def min: A = asA(1)

  def remove(x: A): Unit = {
    val i = referencing.of(x)
    if (i < 1 || i >= _size) {
      throw new IndexOutOfBoundsException(i.toString)
    }
    else {
      swap(i, _size - 1)
      if (i > 1 && ord.lt(asA(i), asA(i / 2))) {
        heapifyUp(i)
      } else {
        heapifyDown(i, _size - 1)
      }
      removeLast()
    }
  }

}
