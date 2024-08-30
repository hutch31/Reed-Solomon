package Rs

import play.api.libs.json._
import java.io.{File, PrintWriter}

// Generic JSON Writer Object
object JsonWriter {
  def writeToFile[T](obj: T, filename: String)(implicit writes: Writes[T]): Unit = {
    val json = Json.prettyPrint(Json.toJson(obj))
    val pw = new PrintWriter(new File(filename))
    try pw.write(json) finally pw.close()
  }
}
