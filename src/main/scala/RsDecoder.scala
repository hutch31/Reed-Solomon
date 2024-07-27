package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsDecoder(c: Config) extends Module {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(c.BUS_WIDTH, c.SYMB_WIDTH)))
    val errPosIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val errValIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val chienErrDetect = Output(Bool())
    val msgCorrupted = Output(Bool())
    val syndValid = Output(Bool())
  })

  val rsSynd = Module(new RsSynd(c))
  val rsBm = Module(new RsBm(c))
  val rsChien = Module(new RsChien(c))
  val rsForney = Module(new RsForney(c))

  rsSynd.io.sAxisIf <> io.sAxisIf
  rsBm.io.syndIf <> rsSynd.io.syndIf
  rsChien.io.errLocIf <> rsBm.io.errLocIf
  rsForney.io.errPosIf <> rsChien.io.errPosIf
  rsForney.io.syndIf.bits := rsSynd.io.syndIf.bits //.reverse
  rsForney.io.syndIf.valid := rsSynd.io.syndIf.valid

  io.chienErrDetect <> rsChien.io.chienErrDetect
  io.errPosIf <> rsChien.io.errPosIf
  io.errValIf <> rsForney.io.errValIf

  // if the syndrome is not zero then block corrupted
  io.msgCorrupted := rsSynd.io.syndIf.bits.reduce(_|_).orR
  io.syndValid := rsSynd.io.syndIf.valid
}

// runMain Rs.GenRsDecoder
//object GenRsDecoder extends App {
//  val config = JsonReader.readConfig("/home/egorman44/chisel-lib/rs.json")
//  ChiselStage.emitSystemVerilogFile(new RsDecoder(config), Array())
//}
