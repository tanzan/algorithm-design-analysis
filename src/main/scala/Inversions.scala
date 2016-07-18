import scala.io.Source

/**
  * Created by serg on 14.07.16.
  */
object Inversions {


  def count(input: Array[Int]):Int = {

    def countAndSort(input:Array[Int], start:Int, end:Int, output:Array[Int]):Int = {

      val mid = (end - start) / 2

      val left = countAndSort(input, start, mid, output)
      val right = countAndSort(input, mid, end, output)
      val split = mergeAndCountSplits(input, start, end, output)

      left + right + split
    }

    def mergeAndCountSplits(input:Array[Int], start:Int, end:Int, output:Array[Int]):Int = {

    }

    countAndSort(input, 0, input.size, Array.ofDim(input.length))
  }

}
