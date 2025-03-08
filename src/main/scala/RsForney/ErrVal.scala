/*
 * ----------------------------------------------------------------------
 *  Copyright (c) 2024 Egor Smirnov
 *
 *  Licensed under terms of the MIT license
 *  See https://github.com/egorman44/Reed-Solomon/blob/main/LICENSE
 *    for license terms
 * ----------------------------------------------------------------------
 */

package Rs

import chisel3._
import chisel3.util._

class ErrVal(c: Config) extends Module {
  val io = IO(new Bundle {
    val formalDerIf    = Input(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
    val errEvalXlInvIf = Input(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val Xl = Input(Vec(c.T_LEN, (UInt(c.SYMB_WIDTH.W))))
    val errValIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val errValShiftCompleted = Output(Bool())
  })

  ///////////////////////////
  // Adjust Xl
  ///////////////////////////

  val XlAdj = Wire(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
  for(i <- 0 until c.T_LEN) {
    if(c.FCR_SYMB == 1) {
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

  val shiftMod = Module(new ShiftBundleMod(new ShiftUnit, c.T_LEN, c.forneyEvTermsPerCycle))

  // Map inputs
  shiftMod.io.vecIn.valid := io.errEvalXlInvIf.valid
  for(i <- 0 until c.T_LEN) {
    shiftMod.io.vecIn.bits(i).formDerSymb := io.formalDerIf(i)
    shiftMod.io.vecIn.bits(i).errEvalXlInvSymb := io.errEvalXlInvIf.bits.vec(i)
    shiftMod.io.vecIn.bits(i).XlSymb := XlAdj(i)
  }

  // Map outputs
  val formDerShift = VecInit(shiftMod.io.vecOut.bits.map(_.formDerSymb))
  val errEvalXlInvShift = VecInit(shiftMod.io.vecOut.bits.map(_.errEvalXlInvSymb))
  val XlShift = VecInit(shiftMod.io.vecOut.bits.map(_.XlSymb))

  ///////////////////////////
  // Combo Stage
  ///////////////////////////

  val errEvalXlInvAdj = Wire(Vec(c.forneyEvTermsPerCycle, UInt(c.SYMB_WIDTH.W)))
  val errValStageOut = Wire(Vec(c.forneyEvTermsPerCycle, UInt(c.SYMB_WIDTH.W)))

  for(i <- 0 until c.forneyEvTermsPerCycle) {
    errEvalXlInvAdj(i) := c.gfMult(errEvalXlInvShift(i), XlShift(i))
    errValStageOut(i) := c.gfDiv(errEvalXlInvAdj(i), formDerShift(i))
  }

  ///////////////////////////
  // Accum Vector
  ///////////////////////////

  // The matrix fully loaded into accumMat when lastQ is asserted

  val lastQ = RegNext(shiftMod.io.lastOut)

  val errValAccumVec = Reg(Vec(c.forneyEvShiftLatency, Vec(c.forneyEvTermsPerCycle, UInt(c.SYMB_WIDTH.W))))
  val errValVec = Wire(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
  
  for(i <- 0 until c.forneyEvShiftLatency) {
    when(shiftMod.io.vecOut.valid){
      if(i == 0)
        errValAccumVec(c.forneyEvShiftLatency-1) := errValStageOut
      else
        errValAccumVec(c.forneyEvShiftLatency-1-i) := errValAccumVec(c.forneyEvShiftLatency-i)
    }
  }

  // Flatten 2D errValAccumVec(i)(j) into 1D errValVec(idx)
  // where idx = i * c.forneyEvTermsPerCycle + j
  for (idx <- 0 until c.T_LEN) {
    // Compute row/col from idx
    val i = idx / c.forneyEvTermsPerCycle
    val j = idx % c.forneyEvTermsPerCycle
    errValVec(idx) := errValAccumVec(i)(j)
  }

  ///////////////////////////////////
  // Output signal
  ///////////////////////////////////

  io.errValIf.bits.vec := errValVec
  // Don't need ffs, since it's not used in ErrVal block
  // and RsBlockRecovery uses ffs from RsChien.
  io.errValIf.bits.ffs := 0.U
  io.errValIf.valid := lastQ

  io.errValShiftCompleted := io.errEvalXlInvIf.valid //shiftMod.io.lastOut

  /////////////////
  // Assert not ready
  /////////////////
  val notReadyAssrt = Module(new NotReadyAssrt(true))
  notReadyAssrt.io.start := io.errEvalXlInvIf.valid
  notReadyAssrt.io.stop := shiftMod.io.lastOut

}
