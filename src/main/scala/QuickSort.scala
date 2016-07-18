/**
  * Created by serg on 18.07.16.
  */

object QuickSort {


  def sort(input:Array[Int], start:Int, end:Int, choosePivot:(Int, Int, Array[Int]) => Int):Unit = {

    def swap(i:Int, j:Int):Unit = {
      val tmp = input(i)
      input(i) = input(j)
      input(j) = tmp
    }

    if (end - start <= 1) ()
    else {
      val pivot = choosePivot(start, end, input)
      var i = start + 1

      for (j <- start until end) {
        if (input(j) < input(pivot)) {
          swap(i, j)
          i += 1
        }
      }
      swap(pivot, i - 1)

      sort(input, start, i - 1, choosePivot)
      sort(input, i, end, choosePivot)
    }
  }

  def sortWithStartPivot(input:Array[Int]):Unit = sort(input, 0, input.length, (start, end, input) => start)

  def main(args: Array[String]) {
    val input = Util.readFromFile("QuickSort.txt")
    sortWithStartPivot(input)
    input.foreach(println)
  }

}
