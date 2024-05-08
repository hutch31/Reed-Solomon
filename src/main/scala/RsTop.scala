package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsTop extends Module with GfParams {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(axisWidth)))
    val errLocIf = Output(Valid(Vec(tLen+1, UInt(symbWidth.W))))
  })

  val rsSynd = Module(new RsSynd)
  val rsBm = Module(new RsBm)

  rsSynd.io.sAxisIf <> io.sAxisIf
  rsBm.io.syndIf <> rsSynd.io.syndIf
  io.errLocIf <> rsBm.io.errLocIf

}

// runMain Rs.GenRs
object GenRs extends App {
  ChiselStage.emitSystemVerilogFile(new RsTop(), Array())
}
