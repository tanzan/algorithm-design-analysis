import scala.io.Source

/**
  * Created by serg on 18.07.16.
  */
object Util {

  def readFromFile(fileName:String):Array[Int] =
    Source.fromFile(fileName).getLines().map(_.toInt).toArray


}
