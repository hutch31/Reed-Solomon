package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsSynd extends Module with GfParams {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(axisWidth)))
    val synd = Output(Valid(Vec(redundancy, UInt(symbWidth.W))))
  })

  for(i <- 0 until redundancy) {
    val rsSyndRoot = Module(new RsSyndPolyEval)
    rsSyndRoot.io.root := alphaToSymb(i.U)
    rsSyndRoot.io.sAxisIf := io.sAxisIf
    io.synd.bits(i) := rsSyndRoot.io.synd.bits
    io.synd.valid := rsSyndRoot.io.synd.valid
  }
}

// runMain Rs.GenSynd
object GenSynd extends App {
  //ChiselStage.emitSystemVerilogFile(new RsSyndHorner(), Array())
  //ChiselStage.emitSystemVerilogFile(new GfPolyTermEval(), Array())
  ChiselStage.emitSystemVerilogFile(new RsSyndPolyEval(), Array())
  ChiselStage.emitSystemVerilogFile(new RsSynd(), Array())
}
