package algo

import scala.io.Source
import scala.reflect.ClassTag

/**
  * Created by serg on 18.07.16.
  */
object Util {

 implicit def stringToInt(s:String):Int = java.lang.Integer.parseInt(s)

 implicit def stringToLong(s:String):Long = java.lang.Long.parseLong(s)

  def readArrayFromFile[T:ClassTag](fileName:String)(implicit convert: String => T):Array[T] =
    Source.fromFile(fileName).getLines().map(x => convert(x)).toArray
}
