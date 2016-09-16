package algo.localsearch

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
  * Created by serg on 9/14/16.
  */
object TwoSAT {

  val random = Random

  trait Term {

    def apply(): Boolean

    def flip():Unit

    def onChange(action: => Unit):Unit
  }

  class Var extends Term {

    private var value = false

    private val actions = mutable.Set[() => Unit]()

    override def apply(): Boolean = value

    def update(value: Boolean): Unit =
      if (this() != value){
        this.value = value
        actions.foreach(_())
      }

    override def flip():Unit = this() = !value

    override def onChange(action: => Unit): Unit = actions += (() => action)
  }

  class Not(val variable:Var) extends Term {

    override def apply(): Boolean = !variable()

    override def flip(): Unit = variable.flip()

    override def onChange(action: => Unit): Unit = variable.onChange(action)
  }

  class Clause(val x:Term, val y:Term, expression: Expression) {

    x.onChange(updateExpression())
    y.onChange(updateExpression())

    private var oldValue = this()

    private def updateExpression():Unit =
      if (this() != oldValue) {
        if (this ()) expression._unsatisfied -= this
        else expression._unsatisfied += this
        oldValue = this()
      }

    def randomFlip():Unit =
      if (random.nextBoolean()) x.flip()
      else y.flip()

    def apply():Boolean = x() || y()
  }

  class Expression {

    private val varMap = mutable.Map[Int, Var]()

    private val _clauses = mutable.ArrayBuffer[Clause]()

    private[TwoSAT] val _unsatisfied = mutable.HashSet[Clause]()

    val vars:Iterable[Var] = varMap.values

    val clauses:Iterable[Clause] = _clauses

    val unsatisfied:Iterable[Clause] = _unsatisfied

    def addClause(x:Int, y:Int):Unit = {
      val xv = varMap.getOrElseUpdate(Math.abs(x), new Var())
      val yv = varMap.getOrElseUpdate(Math.abs(y), new Var())

      val clause = new Clause(if (x > 0) xv else new Not(xv), if (y > 0) yv else new Not(yv), this)
      _clauses += clause
      if (!clause()) _unsatisfied +=  clause
    }

    def apply():Boolean =_unsatisfied.isEmpty
  }

  def papadimitrou(expr: Expression):Boolean = {

    val m = (Math.log(expr.vars.size)/Math.log(2)).toLong
    val n = 2L*expr.vars.size.toLong*expr.vars.size.toLong

    var i = 0L

    while(i < m){
      expr.vars.foreach(_() = random.nextBoolean())
      var j = 0
      while (j < n) {
        if (expr()) return true
        expr.unsatisfied.head.randomFlip()
        j += 1
      }
      i += 1
    }

    false
  }

  def readExpression(fileName:String):Expression = {
    val expr = new Expression
    Source.fromFile(fileName).getLines().drop(1).foreach{ line =>
      val vars = line.split("\\s").map(_.toInt)
      expr.addClause(vars(0), vars(1))
    }
    expr
  }


  def main(args: Array[String]): Unit = {
    val cs1 = readExpression("2sat1.txt")
    println(papadimitrou(cs1) == true)

    val cs2 = readExpression("2sat2.txt")
    println(papadimitrou(cs2) == false)

    val cs3 = readExpression("2sat3.txt")
    println(papadimitrou(cs3) == true)

    val cs4 = readExpression("2sat4.txt")
    println(papadimitrou(cs4) == true)

    val cs5 = readExpression("2sat5.txt")
    println(papadimitrou(cs5) == false)

    val cs6 = readExpression("2sat6.txt")
    println(papadimitrou(cs6) == false)

    //101100
  }

}
