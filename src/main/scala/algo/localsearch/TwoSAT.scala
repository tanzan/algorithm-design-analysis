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

    def onChange(action: => Unit):Unit
  }

  class Instance extends Var {

    private var value = false

    private val actions = mutable.Set[() => Unit]()

    override def apply(): Boolean = value

    override def update(value: Boolean): Unit = {
      this.value = value
      actions.foreach(_())
    }

    override def flip():Unit = this() = !value

    override def onChange(action: => Unit): Unit = actions += (() => action)
  }

  class Not(val variable:Var) extends Var {

    override def apply(): Boolean = !variable()

    override def update(value: Boolean): Unit =  variable() = value

    override def flip(): Unit = variable.flip()

    override def onChange(action: => Unit): Unit = variable.onChange(action)
  }

  class Clause(val x:Var, val y:Var, expression: Expression) {

    x.onChange(updateExpression())
    y.onChange(updateExpression())

    private def updateExpression():Unit =
      if (this()) expression._unsatisfied -= this
      else expression._unsatisfied += this

    def randomFlip():Unit =
      if (random.nextBoolean()) x.flip()
      else y.flip()

    def apply():Boolean = x() || y()
  }

  class Expression {

    private val varMap = mutable.Map[Int, Var]()

    private val _clauses = mutable.ArrayBuffer[Clause]()

    private[TwoSAT] val _unsatisfied = mutable.Set[Clause]()

    val vars:Iterable[Var] = varMap.values

    val clauses:Iterable[Clause] = _clauses

    val unsatisfied:Iterable[Clause] = _unsatisfied

    def addClause(x:Int, y:Int):Unit = {
      val xv = varMap.getOrElseUpdate(Math.abs(x), new Instance())
      val yv = varMap.getOrElseUpdate(Math.abs(y), new Instance())

      val clause = new Clause(if (x > 0) xv else new Not(xv), if (y > 0) yv else new Not(yv), this)
      _clauses += clause
      _unsatisfied +=  clause
    }

    def apply():Boolean = unsatisfied.isEmpty
  }

  def papadimitrou(expr: Expression):Boolean = {

    for(i <- 1 to (Math.log(expr.vars.size)/Math.log(2)).toInt){
      expr.vars.foreach(_() = random.nextBoolean())
      for(j <- 1 to 2*expr.vars.size*expr.vars.size) {
        if (expr()) return true
        expr.unsatisfied.head.randomFlip()
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
    println(papadimitrou(cs1) == true)

    val cs2 = readClauses("2sat2.txt")
    println(papadimitrou(cs2) == false)

    val cs3 = readClauses("2sat3.txt")
    println(papadimitrou(cs3) == true)

    val cs4 = readClauses("2sat4.txt")
    println(papadimitrou(cs4) == true)

    val cs5 = readClauses("2sat5.txt")
    println(papadimitrou(cs5) == false)

    val cs6 = readClauses("2sat6.txt")
    println(papadimitrou(cs6) == false)

    //101100
  }

}
