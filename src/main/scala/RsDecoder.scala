package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsDecoder extends Module with GfParams {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(axisWidth)))
    val errPosIf = Output(Valid(new vecFfsIf(tLen)))
    val errValIf = Output(Valid(new vecFfsIf(tLen)))
    val chienErrDetect = Output(Bool())
    val msgCorrupted = Output(Bool())
    val syndValid = Output(Bool())
  })

  val rsSynd = Module(new RsSynd)
  val rsBm = Module(new RsBm)
  val rsChien = Module(new RsChien)
  val rsForney = Module(new RsForney)

  rsSynd.io.sAxisIf <> io.sAxisIf
  rsBm.io.syndIf <> rsSynd.io.syndIf
  rsChien.io.errLocIf <> rsBm.io.errLocIf
  rsForney.io.errPosIf <> rsChien.io.errPosIf
  rsForney.io.syndIf.bits := rsSynd.io.syndIf.bits //.reverse
  rsForney.io.syndIf.valid := rsSynd.io.syndIf.valid

  io.chienErrDetect <> rsChien.io.chienErrDetect
  io.errPosIf <> rsForney.io.errPosOutIf
  io.errValIf <> rsForney.io.errValIf

  // if the syndrome is not zero then block corrupted
  io.msgCorrupted := rsSynd.io.syndIf.bits.reduce(_|_).orR
  io.syndValid := rsSynd.io.syndIf.valid
}

// runMain Rs.GenRsDecoder
object GenRsDecoder extends App {
  ChiselStage.emitSystemVerilogFile(new RsDecoder(), Array())
}
