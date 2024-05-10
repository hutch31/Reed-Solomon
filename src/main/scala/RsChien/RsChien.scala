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
    val errLocIf = Input(Valid(new vecFfsIf(tLen+1)))
    val posArray = Output(new NumPosIf)
    val chienErrDetect = Output(Bool())
  })

  val rsChienErrBitPos = Module(new RsChienErrBitPos)
  val rsChienBitPosToNum = Module(new RsChienBitPosToNum)

  rsChienErrBitPos.io.errLocIf <> io.errLocIf
  rsChienBitPosToNum.io.bitPos <> rsChienErrBitPos.io.bitPos
  io.posArray <> rsChienBitPosToNum.io.posArray

  when(io.posArray.valid) {
    io.chienErrDetect := (io.errLocIf.bits.ffs(tLen,1) ^ io.posArray.sel).orR
  }.otherwise {
    io.chienErrDetect := 0.U
  }
}

//
// runMain Rs.GenChien
object GenChien extends App with GfParams{
  //ChiselStage.emitSystemVerilogFile(new GfPolyEvalHorner(tLen, ffStepPolyEval), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChienErrBitPos(), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChienBitPosToNum(), Array())
  //ChiselStage.emitSystemVerilogFile(new FindFirstSetNew(width=chienRootsPerCycle, lsbFirst=true), Array())
  ChiselStage.emitSystemVerilogFile(new RsChien(), Array())
}
