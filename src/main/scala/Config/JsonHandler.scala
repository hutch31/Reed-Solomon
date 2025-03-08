/*
 * ----------------------------------------------------------------------
 *  Copyright (c) 2024 Egor Smirnov
 *
 *  Licensed under terms of the MIT license
 *  See https://github.com/egorman44/Reed-Solomon/blob/main/LICENSE
 *    for license terms
 * ----------------------------------------------------------------------
 */

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

  implicit val configReads: Reads[CmdConfig] = Json.reads[CmdConfig]

  def readConfig(filePath: String): Config = {
    val source = Source.fromFile(filePath)
    val jsonString = try source.mkString finally source.close()
    val cmdConfig = Json.parse(jsonString).as[CmdConfig]
    Config(cmdConfig)
  }
}
