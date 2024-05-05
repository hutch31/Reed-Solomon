package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsSynd extends Module with GfParams {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(axisWidth)))
    val syndIf = Output(Valid(Vec(redundancy, UInt(symbWidth.W))))
  })

  for(i <- 0 until redundancy) {
    val rsSyndRoot = Module(new RsSyndPolyEval)    
    rsSyndRoot.io.root := alphaToSymb((firstConsecutiveRoot+i).U)
    rsSyndRoot.io.sAxisIf := io.sAxisIf
    io.syndIf.bits(i) := rsSyndRoot.io.syndIf.bits
    io.syndIf.valid := rsSyndRoot.io.syndIf.valid
  }
}

// runMain Rs.GenSynd
object GenSynd extends App {
  //ChiselStage.emitSystemVerilogFile(new RsSyndHorner(), Array())
  //ChiselStage.emitSystemVerilogFile(new GfPolyTermEval(), Array())
  //ChiselStage.emitSystemVerilogFile(new RsSyndPolyEval(), Array())
  ChiselStage.emitSystemVerilogFile(new RsSynd(), Array())
}
