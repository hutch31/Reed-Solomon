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
        .optional()
        .action((x, c) => c.copy(AXIS_CLOCK = x))
        .text("axis-clock defines the frequency of the input interface"),

      opt[Double]("core-clock")
        .optional()
        .action((x, c) => c.copy(CORE_CLOCK = x))
        .text("core-clock defines the frequency of the core clock that is used for RsBm, RsChien, RsForney."),

      opt[Int]("symb-width-in-bits")
        .required()
        .action((x, c) => c.copy(SYMB_WIDTH = x))
        .text("symb-width-in-bits defines the width of the symbol in bits"),

      opt[Int]("bus-width-in-symb")
        .required()
        .action((x, c) => c.copy(BUS_WIDTH = x))
        .text("bus-width-in-symb defines the input bus width in symbols"),

      opt[Int]("poly")
        .required()
        .action((x, c) => c.copy(POLY = x))
        .text("poly defines the polynomial you use to create a GF field"),

      opt[Int]("fcr")
        .required()
        .action((x, c) => c.copy(FCR = x))
        .text("fcr is a first consecutive root"),

      opt[Int]("n-len")
        .required()
        .action((x, c) => c.copy(N_LEN = x))
        .text("n-len specifies the length of the encoded message"),

      opt[Int]("k-len")
        .required()
        .action((x, c) => c.copy(K_LEN = x))
        .text("k-len specifies the length of the original message")
    )
  }

  def parse(args: Array[String]): Option[CmdConfig] = {
    OParser.parse(parser, args, CmdConfig(156.25, 156.25, 0, 0, 0, 0, 0, 0))
  }
}
