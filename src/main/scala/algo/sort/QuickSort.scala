package algo.sort

import algo.Util

import java.util
import scala.reflect.ClassTag
import scala.util.Random

object QuickSort {


  def sort(input:Array[Int], start:Int, end:Int,
           choosePivot:(Int, Int, Array[Int]) => Int):Int = {

    def swap(i:Int, j:Int):Unit = {
      val tmp = input(i)
      input(i) = input(j)
      input(j) = tmp
    }

    if (end - start <= 1) 0
    else {
      val pivot = choosePivot(start, end, input)
      swap(start, pivot)
      var i = start + 1

      for (j <- start until end) {
        if (input(j) < input(start)) {
          swap(i, j)
          i += 1
        }
      }
      swap(start, i - 1)

      end - start - 1 +
        sort(input, start, i - 1 , choosePivot) +
        sort(input, i , end, choosePivot)
    }
  }


  def sort[T : Ordering : ClassTag](input: Array[T]): Array[T] = {
    val ord = implicitly[Ordering[T]]
    val output = Array.copyAs[T](input, input.length)

    def swap(i: Int, j: Int): Unit = {
      val tmp = output(i)
      output(i) = output(j)
      output(j) = tmp
    }

    def partition(low: Int, hi: Int): Int = {
      val pivot = hi
      var i = low
      for {
        j <- low to hi
        if ord.lt(output(j), output(pivot))
      } {
        swap(i, j)
        i += 1
      }
      swap(i, pivot)
      i
    }

    def doSort(low: Int, hi: Int): Unit = {
      var stack = (low, hi) :: Nil
      while (stack.nonEmpty) {
        val (low, hi) = stack.head
        stack = stack.tail
        if (low < hi) {
          val p = partition(low, hi)
          stack = (low, p - 1) :: stack
          stack = (p + 1, hi) :: stack
        }
      }
    }

    doSort(0, input.length - 1)
    output
  }

  def sortWith(array:Array[Int], choosePivot:(Int, Int, Array[Int]) => Int):Int =
    sort(array, 0, array.length, choosePivot)

  def startPivot(start:Int, end:Int, input:Array[Int]):Int = start

  def endPivot(start:Int, end:Int, input:Array[Int]):Int = end - 1

  def medianPivot(start:Int, end:Int, input:Array[Int]):Int = {
    val mid =  start + (end - start + 1)/2  - 1
    if (input(start) < input(end - 1)) {
      if (input(mid) < input(start)) start
      else if (input(mid) > input(end - 1)) end - 1
      else mid
    } else {
      if (input(mid) < input(end - 1)) end - 1
      else if(input(mid) > input(start)) start
      else mid
    }
  }

  val rng = Random

  def randomPivot(start:Int, end:Int, input:Array[Int]):Int = start + rng.nextInt(end - start)

  def main(args: Array[String]): Unit = {
    import Util._
    val input = readArrayFromFile[Int]("QuickSort.txt")
    val a1 = input.map(x => x)
    val a2 = input.map(x => x)
    val a3 = input.map(x => x)
    val a4 = input.map(x => x).sortWith(_ < _)
    val a5 = input.map(x => x)

    println(sortWith(a1, startPivot))
    println(sortWith(a2, endPivot))
    println(sortWith(a3, medianPivot))
    println(sortWith(a5, randomPivot))
    println(sort(a1).toList)
    println(sort(Array(1)).toList)
    println(sort(Array(2,1)).toList)
    println(sort(Array[Int]()).toList)
    println(sort(Array(1,2,3,4,5)).toList)

    println(input.indices.forall(i => a1(i) == a2(i) && a2(i) == a3(i) && a3(i) == a4(i) && a5(i) == a4(i)))
  }

}
