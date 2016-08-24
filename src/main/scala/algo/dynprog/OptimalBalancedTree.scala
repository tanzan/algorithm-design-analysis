package algo.dynprog

/**
  * Created by serg on 24.08.16.
  */
object OptimalBalancedTree {

  def computeOptimalExpectation(probs:Array[Double]):Double = {
    val expectations = Array.ofDim[Double](probs.size, probs.size)
    def get(i:Int,j:Int):Double = {
      if (j < 0 || j >= probs.size || i < 0 || i >= probs.size) 0
      else expectations(i)(j)
    }
    for(s <- 0 until probs.size){
      for(i <- 0 until probs.size) {
        var p = 0.0
        for(k <- i to i + s){
          p += (if (k >= probs.size) 0.0 else probs(k))
        }
        var e = Double.MaxValue
        for(r <- i to i + s) {
          e = Math.min(e, p + get(i, r - 1) + get(r + 1,i + s))
        }
        if (i < 0 || i >= probs.size || i + s < 0 || i + s >= probs.size) ()
        else expectations(i)(i + s) = e
      }
    }
    expectations(0)(probs.size - 1)
  }


  def main(args: Array[String]): Unit = {
    println(computeOptimalExpectation(Array(0.05, 0.4, 0.08, 0.04, 0.1, 0.1, 0.23)))
  }

}
