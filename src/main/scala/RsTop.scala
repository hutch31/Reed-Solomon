package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsTop extends Module with GfParams {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(axisWidth)))
    val errPosIf = Output(Valid(new vecFfsIf(tLen)))
    val chienErrDetect = Output(Bool())
  })

  val rsSynd = Module(new RsSynd)
  val rsBm = Module(new RsBm)
  val rsChien = Module(new RsChien)

  rsSynd.io.sAxisIf <> io.sAxisIf
  rsBm.io.syndIf <> rsSynd.io.syndIf  
  rsChien.io.errLocIf <> rsBm.io.errLocIf

  io.errPosIf <> rsChien.io.errPosIf
  io.chienErrDetect <> rsChien.io.chienErrDetect

}

// runMain Rs.GenRsTop
object GenRsTop extends App {
  ChiselStage.emitSystemVerilogFile(new RsTop(), Array())
}
