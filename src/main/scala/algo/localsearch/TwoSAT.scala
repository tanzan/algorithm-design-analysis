package algo.localsearch

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
  * Created by serg on 9/14/16.
  */
object TwoSAT {

  val random = Random

  trait Var {

    def apply(): Boolean

    def update(value: Boolean): Unit

    def flip():Unit
  }

  class Instance extends Var {

    private var value = false

    override def apply(): Boolean = value

    override def update(value: Boolean): Unit = this.value = value

    override def flip():Unit = value = !value

  }

  class Not(val variable:Var) extends Var {

    override def apply(): Boolean = !variable()

    override def update(value: Boolean): Unit =  variable() = (value)

    override def flip(): Unit = variable.flip()
  }

  class Clause(val x:Var, val y:Var) {
    def randomFlip():Unit =
      if (random.nextBoolean()) x.flip()
      else y.flip()

    def apply():Boolean = x() || y()
  }

  class Expression {

    private val varMap = mutable.Map[Int, Var]()

    private val _clauses = mutable.ArrayBuffer[Clause]()

    val vars:Iterable[Var] = varMap.values

    val clauses:Seq[Clause] = _clauses

    def addClause(x:Int, y:Int):Unit = {
      val xv = varMap.getOrElseUpdate(Math.abs(x), new Instance())
      val yv = varMap.getOrElseUpdate(Math.abs(y), new Instance())

      _clauses += new Clause(if (x > 0) xv else new Not(xv), if (y > 0) yv else new Not(yv))
    }

    def apply():Boolean = clauses.forall(_())
  }

  def papadimitrou(expr: Expression):Boolean = {

    for(i <- 1 to (Math.log(expr.vars.size)/Math.log(2)).toInt){
      expr.vars.foreach(_() = random.nextBoolean())
      for(j <- 1 to 2*expr.vars.size*expr.vars.size) {
        if (expr()) return true
        val unsatisfied = expr.clauses.filter(!_())
        unsatisfied(random.nextInt(unsatisfied.size)).randomFlip()
      }
    }

    false
  }

  def readClauses(fileName:String):Expression = {
    val expr = new Expression
    Source.fromFile(fileName).getLines().drop(1).foreach{ line =>
      val vars = line.split("\\s+").map(_.toInt)
      expr.addClause(vars(0), vars(1))
    }
    expr
  }


  def main(args: Array[String]): Unit = {
    val cs1 = readClauses("2sat1.txt")
    println(cs1.vars.size)
    println(papadimitrou(cs1))

    //val cs2 = readClauses("2sat2.txt")
    //println(papadimitrou(cs2))

    //val cs3 = readClauses("2sat3.txt")
    //println(papadimitrou(cs3))

    //val cs4 = readClauses("2sat4.txt")
    //println(papadimitrou(cs4))

    //val cs5 = readClauses("2sat5.txt")
    //println(papadimitrou(cs5))

    //val cs6 = readClauses("2sat6.txt")
    //println(papadimitrou(cs6))
  }

}
