import scala.io.Source

/**
  * Created by serg on 14.07.16.
  */
object Inversions {

  def readFromFile(fileName:String):Array[Int] =
    Source.fromFile(fileName).getLines().map(_.toInt).toArray

  def count(input: Array[Int]):Long = {

    def countAndSort(input:Array[Int], start:Int, end:Int):(Long, Array[Int]) = {
      if (end - start <= 1) (0, input.slice(start, end))
      else {
        val mid = (end - start) / 2

        val (leftN, leftArr) = countAndSort(input, start, start + mid)
        val (rightN, rightArr) = countAndSort(input, start + mid, end)
        val (splitN, arr) = mergeAndCountSplits(leftArr, rightArr)

        (leftN + rightN + splitN, arr)
      }
    }

    def mergeAndCountSplits(left:Array[Int], right: Array[Int]):(Long, Array[Int]) = {
      var n = 0L
      var i = 0
      var j = 0
      val output = Array.ofDim[Int](left.length + right.length)
      for(k <- 0 until output.length) {
        if (j >= right.length || (i < left.length && left(i) <= right(j))) {
          output(k) = left(i)
          i += 1
        } else {
          output(k) = right(j)
          j += 1
          n += left.length - i
        }
      }
      (n, output)
    }

    countAndSort(input, 0, input.length)._1
  }

  def main(args: Array[String]) {
    val arr = readFromFile("IntegerArray.txt")
    println(count(arr))

  }

}
