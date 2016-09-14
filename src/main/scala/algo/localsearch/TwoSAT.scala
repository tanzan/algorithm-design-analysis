package algo.localsearch

/**
  * Created by serg on 9/14/16.
  */
object TwoSAT {

  case class Clause(x:Int, y:Int) {
    def randomFlip() = ???
  }

  def papadimitrou(clauses:Seq[Clause]):Boolean = {

    def chooseRandomAssignment():Seq[Clause] = ???

    def chooseRandomClause(assignment:Seq[Clause]):Clause = ???

    for(i <- 1 to (Math.log(clauses.size)/Math.log(2)).toInt){
      val assignment = chooseRandomAssignment()
      for(j <- 1 to 2*clauses.size*clauses.size) {
        if (clauses.zip(assignment).forall(pair => pair._1 == pair._2)) return true
        val clause = chooseRandomClause(assignment)
        clause.randomFlip();
      }
    }
  }

}
