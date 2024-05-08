package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

/////////////////////////////////////////
// RsChien
/////////////////////////////////////////

class RsChien extends Module with GfParams{
  val io = IO(new Bundle {
    val errLocIf = Input(Valid(Vec(tLen+1, UInt(symbWidth.W))))   
    val posArray = Output(new NumPosIf)
  })

  val rsChienErrBitPos = Module(new RsChienErrBitPos)
  val rsChienBitPosToNum = Module(new RsChienBitPosToNum)

  rsChienErrBitPos.io.errLocIf := io.errLocIf
  rsChienBitPosToNum.io.bitPos := rsChienErrBitPos.io.bitPos
  io.posArray := rsChienBitPosToNum.io.posArray
}

//
// runMain Rs.GenChien
object GenChien extends App with GfParams{
  //ChiselStage.emitSystemVerilogFile(new GfPolyEvalHorner(tLen, ffStepPolyEval), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChienErrBitPos(), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChienBitPosToNum(), Array())
  ChiselStage.emitSystemVerilogFile(new FindFirstSetNew(width=chienRootsPerCycle, lsbFirst=true), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChien(), Array())
}
