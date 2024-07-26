package Rs

import chisel3._
import chisel3.util._

class ErrVal(c: Config) extends Module {
  val io = IO(new Bundle {
    val formalDerIf    = Input(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
    val errEvalXlInvIf = Input(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val Xl = Input(Vec(c.T_LEN, (UInt(c.SYMB_WIDTH.W))))
    val errValIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
  })

  ///////////////////////////
  // Adjust Xl
  ///////////////////////////

  val XlAdj = Wire(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
  for(i <- 0 until c.T_LEN) {
    if(c.FIRST_ROOT_POWER == 1) {
      XlAdj := io.Xl
    } else {
      for(i <- 0 until c.T_LEN){
        XlAdj(i) := c.powerFirstRootMin1(io.Xl(i))
      }
    }
  }

  ///////////////////////////
  // Shift vectors
  ///////////////////////////

  class ShiftUnit extends Bundle {
    val formDerSymb = UInt(c.SYMB_WIDTH.W)
    val errEvalXlInvSymb = UInt(c.SYMB_WIDTH.W)
    val XlSymb = UInt(c.SYMB_WIDTH.W)
  }

  val shiftMod = Module(new ShiftBundleMod(new ShiftUnit, c.T_LEN, c.numOfSymbEv))

  // Map inputs
  shiftMod.io.vecIn.valid := io.errEvalXlInvIf.valid
  for(i <- 0 until c.T_LEN) {
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

  val errEvalXlInvAdj = Wire(Vec(c.numOfSymbEv, UInt(c.SYMB_WIDTH.W)))
  val errValStageOut = Wire(Vec(c.numOfSymbEv, UInt(c.SYMB_WIDTH.W)))
  
  for(i <- 0 until c.numOfSymbEv) {
    errEvalXlInvAdj(i) := c.gfMult(errEvalXlInvShift(i), XlShift(i))
    errValStageOut(i) := c.gfDiv(errEvalXlInvAdj(i), formDerShift(i))
  }

  ///////////////////////////
  // Accum Vector
  ///////////////////////////

  val numOfCycles = math.ceil(c.T_LEN/c.numOfSymbEv.toDouble).toInt
  // The matrix fully loaded into accumMat when lastQ is asserted

  val lastQ = RegNext(shiftMod.io.lastOut)
  val errValAccumVec = Reg(Vec(numOfCycles, Vec(c.numOfSymbEv, UInt(c.SYMB_WIDTH.W))))
  val errValVec = Wire(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))

  for(i <- 0 until numOfCycles) {
    if(i == 0)
      errValAccumVec(numOfCycles-1) := errValStageOut
    else
      errValAccumVec(numOfCycles-1-i) := errValAccumVec(numOfCycles-i)
    for(k <- 0 until c.numOfSymbEv) {
      if(i*numOfCycles+k < c.T_LEN)
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
