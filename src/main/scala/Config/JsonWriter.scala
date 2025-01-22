package Rs

import play.api.libs.json._
import java.io.{File, PrintWriter}
import scala.io.Source

// Generic JSON Writer Object
object JsonHandler {

  def writeToFile[T](obj: T, filename: String)(implicit writes: Writes[T]): Unit = {
    val json = Json.prettyPrint(Json.toJson(obj))
    val pw = new PrintWriter(new File(filename))
    try pw.write(json) finally pw.close()
  }

  def readFromFile[T](filename: String)(implicit reads: Reads[T]): Option[T] = {
    val source = Source.fromFile(filename)
    try {
      val json = source.getLines().mkString
      Json.parse(json).validate[T] match {
        case JsSuccess(value, _) => Some(value)
        case JsError(errors) =>
          println(s"Error parsing JSON: $errors")
          None
      }
    } catch {
      case ex: Exception =>
        println(s"Error reading file: ${ex.getMessage}")
        None
    } finally source.close()
  }

}

