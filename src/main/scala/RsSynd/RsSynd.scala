package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsSynd(c: Config) extends Module {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIfNew(c.BUS_WIDTH, c.SYMB_WIDTH)))
    val syndIf = Output(Valid(Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W))))
  })

  for(i <- 0 until c.REDUNDANCY) {
    val rsSyndRoot = Module(new RsSyndPolyEval(c))
    rsSyndRoot.io.root := c.alphaToSymb((c.FCR+i).U)
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
  //ChiselStage.emitSystemVerilogFile(new RsSynd(), Array())
}
