package algo.sort

/**
  * Created by serg on 2/6/17.
  */
object MergeSort {


  def sort[T : Ordering](xs:List[T]):List[T] = {

    def merge(xs:List[T], ys:List[T]):List[T] = {
      (xs,ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x::xs1, y::ys1) =>
          if (implicitly[Ordering[T]].lt(x,y)) x::merge(xs1, ys)
          else y::merge(xs, ys1)
      }
    }

    val n = xs.size/2

    if (n == 0) xs
    else {
      val (ys, zs) = xs.splitAt(n)
      merge(sort(ys), sort(zs))
    }
  }

  case class Person(name:String) extends Ordered[Person] {
    override def compare(that: Person): Int = name.compare(that.name)
  }

  def main(args: Array[String]): Unit = {
    println(sort(List(Person("Billy"), Person("April"), Person("Willy"))))
    println(sort(List(5,2,8,9,1)))
  }

}
