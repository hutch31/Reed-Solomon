package Rs

import chisel3._
import chisel3.util._

class ErrVal extends Module with GfParams {
  val io = IO(new Bundle {
    val formalDerIf    = Input(Vec(tLen, UInt(symbWidth.W)))
    //val formalDerIf    = Input(Valid(new vecFfsIf(tLen)))
    val errEvalXlInvIf = Input(Valid(new vecFfsIf(tLen)))
    //val XlIf = Input(Valid(new vecFfsIf(tLen)))
    val Xl = Input(Vec(tLen, (UInt(symbWidth.W))))
    val errValIf = Output(Valid(new vecFfsIf(tLen)))
    val errValStageOut = Output(Vec(numOfSymbEv, UInt(symbWidth.W)))
  })

  ///////////////////////////
  // Adjust Xl
  ///////////////////////////

  val XlAdj = Wire(Vec(tLen, UInt(symbWidth.W)))
  for(i <- 0 until tLen) {
    if(firstRootPower == 1) {
      XlAdj := io.Xl
    } else {
      for(i <- 0 until tLen){
        XlAdj(i) := powerFirstRootMin1(io.Xl(i))
      }
    }
  }

  ///////////////////////////
  // Shift vectors
  ///////////////////////////

  class ShiftUnit extends Bundle {
    val formDerSymb = UInt(symbWidth.W)
    val errEvalXlInvSymb = UInt(symbWidth.W)
    val XlSymb = UInt(symbWidth.W)
  }

  val shiftMod = Module(new ShiftBundleMod(new ShiftUnit, tLen, numOfSymbEv))

  // Map inputs
  shiftMod.io.vecIn.valid := io.errEvalXlInvIf.valid
  for(i <- 0 until tLen) {
    shiftMod.io.vecIn.bits(i).formDerSymb := io.formalDerIf(i)
    shiftMod.io.vecIn.bits(i).errEvalXlInvSymb := io.errEvalXlInvIf.bits.vec(i)
    shiftMod.io.vecIn.bits(i).XlSymb := XlAdj(i)
  }

  // Map outputs
  val formDerShift = shiftMod.io.vecOut.bits.map(_.formDerSymb)
  val errEvalXlInvShift = shiftMod.io.vecOut.bits.map(_.errEvalXlInvSymb)
  val XlShift = shiftMod.io.vecOut.bits.map(_.XlSymb)

  ///////////////////////////
  // Combo Stage
  ///////////////////////////

  val errEvalXlInvAdj = Wire(Vec(numOfSymbEv, UInt(symbWidth.W)))
  val errValStageOut = Wire(Vec(numOfSymbEv, UInt(symbWidth.W)))

  for(i <- 0 until numOfSymbEv) {
    errEvalXlInvAdj(i) := gfMult(errEvalXlInvShift(i), XlShift(i))
    errValStageOut(i) := gfDiv(errEvalXlInvAdj(i), formDerShift(i))
  }

  io.errValStageOut := errValStageOut

  ///////////////////////////
  // Accum Vector
  ///////////////////////////

  val numOfCycles = math.ceil(tLen/numOfSymbEv.toDouble).toInt
  // The matrix fully loaded into accumMat when lastQ is asserted

  val lastQ = RegNext(shiftMod.io.lastOut)
  val errValAccumVec = Reg(Vec(numOfCycles, Vec(numOfSymbEv, UInt(symbWidth.W))))
  val errValVec = Wire(Vec(tLen, UInt(symbWidth.W)))

  for(i <- 0 until numOfCycles) {
    if(i == 0)
      errValAccumVec(numOfCycles-1) := errValStageOut
    else
      errValAccumVec(numOfCycles-1-i) := errValAccumVec(numOfCycles-i)
    for(k <- 0 until numOfSymbEv) {
      if(i*numOfCycles+k < tLen)
        errValVec(i*numOfCycles+k) := errValAccumVec(i)(k)
    }
  }

  ///////////////////////////////////
  // Output signal
  ///////////////////////////////////

  io.errValIf.bits.vec := errValVec
  io.errValIf.bits.ffs := io.errEvalXlInvIf.bits.ffs
  io.errValIf.valid := lastQ

}
