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

import scopt.OParser
import play.api.libs.json.{Json, OFormat}

case class CmdConfig(
  AXIS_CLOCK: Double,
  CORE_CLOCK: Double,
  SYMB_WIDTH: Int,
  BUS_WIDTH: Int,
  POLY: Int,
  FCR: Int,
  N_LEN: Int,
  K_LEN: Int
)

object CmdConfig {
  // generate JSON formatter for the CmdConfig case class
  implicit val configFormat: OFormat[CmdConfig] = Json.format[CmdConfig]
}

object ConfigParser {
  private val builder = OParser.builder[CmdConfig]
  
  val parser: OParser[Unit, CmdConfig] = {
    import builder._
    OParser.sequence(
      programName("ChiselRSConfig"),
      head("ChiselRSConfig", "1.0"),

      opt[Double]("axis-clock")
        .required()
        .action((x, c) => c.copy(AXIS_CLOCK = x))
        .text("AXIS_CLOCK is a required parameter"),

      opt[Double]("core-clock")
        .required()
        .action((x, c) => c.copy(CORE_CLOCK = x))
        .text("CORE_CLOCK is a required parameter"),

      opt[Int]("symb-width-in-bits")
        .required()
        .action((x, c) => c.copy(SYMB_WIDTH = x))
        .text("SYMB_WIDTH is a required parameter"),

      opt[Int]("bus-width-in-symb")
        .required()
        .action((x, c) => c.copy(BUS_WIDTH = x))
        .text("BUS_WIDTH defines the input bus width in symbols"),

      opt[Int]("poly")
        .required()
        .action((x, c) => c.copy(POLY = x))
        .text("POLY is a required parameter"),

      opt[Int]("fcr")
        .required()
        .action((x, c) => c.copy(FCR = x))
        .text("FCR is a required parameter"),

      opt[Int]("n-len")
        .required()
        .action((x, c) => c.copy(N_LEN = x))
        .text("N_LEN is a required parameter"),

      opt[Int]("k-len")
        .required()
        .action((x, c) => c.copy(K_LEN = x))
        .text("K_LEN is a required parameter")
    )
  }

  def parse(args: Array[String]): Option[CmdConfig] = {
    OParser.parse(parser, args, CmdConfig(0.0, 0.0, 0, 0, 0, 0, 0, 0))
  }
}
