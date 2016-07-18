/**
  * Created by serg on 18.07.16.
  */

object QuickSort {


  def sort(input:Array[Int], start:Int, end:Int, choosePivot:(Int, Int) => Int):Unit = {


  }

  def sortWithStartPivot(input:Array[Int]):Unit = sort(input, 0, input.length, (start, end) => start)


}
