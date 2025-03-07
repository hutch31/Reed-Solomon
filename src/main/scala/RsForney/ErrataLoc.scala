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

///////////////////////////
// ErrataLoc with feedback implementation
///////////////////////////

class ErrataLoc(c: Config) extends Module {
  val io = IO(new Bundle {
    val errPosCoefIf = Input(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val errataLocIf = Output(Valid(new vecFfsIf(c.T_LEN+1, c.SYMB_WIDTH)))
  })
  
  // Modules instances
  val stage = for(i <- 0 until c.forneyErrataLocTermsPerCycle) yield Module(new ErrataLocatorStage(c))
  
  // Slice valid bit that goes into comb stage(s)
  val stageOut = Wire(Vec(c.forneyErrataLocTermsPerCycle, (Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))))

  ///////////////////////////
  // Shift vec and ffs
  //
  // number of cycles are defined
  // by the ffs signal. So the latency
  // of shiftVec could vary. 
  ///////////////////////////

  val shiftMod = Module(new ShiftBundleMod(new ShiftUnit, c.T_LEN, c.forneyErrataLocTermsPerCycle))

  class ShiftUnit extends Bundle {
    val symb = UInt(c.SYMB_WIDTH.W)
    val ffs = Bool()
  }

  // Map inputs
  shiftMod.io.vecIn.valid := io.errPosCoefIf.valid
  for(i <- 0 until c.T_LEN) {
    shiftMod.io.vecIn.bits(i).ffs := io.errPosCoefIf.bits.ffs.asTypeOf(Vec(c.T_LEN, Bool()))(i)
    shiftMod.io.vecIn.bits(i).symb := io.errPosCoefIf.bits.vec(i)
  }

  // Ffs defines the end of calculation
  val errPosVldStage = Wire(Vec(c.forneyErrataLocTermsPerCycle, Bool()))
  errPosVldStage := shiftMod.io.vecOut.bits.map(_.ffs)
  dontTouch(errPosVldStage)

  val coefPositionShift = shiftMod.io.vecOut.bits.map(_.symb)

  // If errataLocNum > 1, then
  // ffs is not stable between two consecutive
  // io.errPosCoefIf.valid signals. Therefore, it needs
  // to be captured into a register before being provided
  // to ffsQ. Otherwise, it may capture an incorrect value
  // from io.errPosCoefIf.bits.ffs.

  val ffsShift = Wire(UInt(c.T_LEN.W))

  if(c.errataLocNum == 1) {
    ffsShift := io.errPosCoefIf.bits.ffs
  } else {
    ffsShift := RegEnable(io.errPosCoefIf.bits.ffs, io.errPosCoefIf.valid)
  }

  ///////////////////////////
  // Capture errataLoc value
  //
  // Output value of errata locator is stored in errataLoc register
  // it should be assigned to 1*x^0 + 0*x^1 + ... + 0*x^c.T_LEN when
  // errPosCoefIf.valid asserted.
  ///////////////////////////

  val errataLoc = Reg(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))

  when (io.errPosCoefIf.valid) {
    for(i <- 0 until c.T_LEN+1) {
      if(i == 0)
        errataLoc(i) := 1.U
      else
        errataLoc(i) := 0.U
    }
  }.otherwise {
    // Capture errataLoc value
    when(errPosVldStage.reduce(_ || _) === 0.U) {
      errataLoc := stage(c.forneyErrataLocTermsPerCycle-1).io.errataLoc
    }.otherwise {      
      if(c.forneyErrataLocTermsPerCycle == 1) {
        errataLoc := stage(c.forneyErrataLocTermsPerCycle-1).io.errataLoc
      }
      else {
        errataLoc := Mux1H(errPosVldStage, stageOut)
      }
    }
  }

  stage(0).io.errataLocPrev := errataLoc
  stage(0).io.coefPosition := coefPositionShift(0)
  stageOut(0) := stage(0).io.errataLoc

  if(c.forneyErrataLocTermsPerCycle > 1) {
    for(i <- 1 until c.forneyErrataLocTermsPerCycle) {
      stage(i).io.errataLocPrev := stage(i-1).io.errataLoc
      stage(i).io.coefPosition := coefPositionShift(i)
      stageOut(i) := stage(i).io.errataLoc
    }
  }

  ////////////////////////////////////
  // Constant latency.
  // 
  // Capture errataLoc and Ffs into separate register to
  // make the block latency a constant.
  ////////////////////////////////////

  val errataLocVld = RegNext(next=shiftMod.io.lastOut, init=false.B)
  val ffsQ = Reg(UInt(c.T_LEN.W))
  val errataLocQ = Reg(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))

  when(errPosVldStage.reduce(_ || _) === 1.U && shiftMod.io.vecOut.valid) {
    //ffsQ := io.errPosCoefIf.bits.ffs
    ffsQ := ffsShift
    if(c.forneyErrataLocTermsPerCycle == 1) {
      errataLocQ := stage(c.forneyErrataLocTermsPerCycle-1).io.errataLoc
    }
    else {
      errataLocQ := Mux1H(errPosVldStage, stageOut)
    }
  }

  // Output assignment
  io.errataLocIf.valid := errataLocVld
  io.errataLocIf.bits.vec := errataLocQ
  io.errataLocIf.bits.ffs := ffsQ

  /////////////////
  // Assert not ready
  /////////////////
  val notReadyAssrt = Module(new NotReadyAssrt())
  notReadyAssrt.io.start := io.errPosCoefIf.valid
  notReadyAssrt.io.stop := errPosVldStage.reduce(_ || _)

}

class ErrataLocatorStage(c: Config) extends Module{
  val io = IO(new Bundle {
    val errataLocPrev = Input(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
    val coefPosition = Input(UInt(c.SYMB_WIDTH.W))
    val errataLoc = Output(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  })

  val coefPositionPower = Wire(UInt(c.SYMB_WIDTH.W))
  dontTouch(coefPositionPower)

  coefPositionPower := c.powerFirstRoot(io.coefPosition)

  for(symbIndx <- 0 until c.T_LEN+1) {
    if(symbIndx == 0)
      io.errataLoc(symbIndx) := 1.U
    else if(symbIndx == c.T_LEN)
      io.errataLoc(symbIndx) := c.gfMult(io.errataLocPrev(symbIndx-1), coefPositionPower)
    else
      io.errataLoc(symbIndx) := c.gfMult(io.errataLocPrev(symbIndx-1), coefPositionPower) ^ io.errataLocPrev(symbIndx)
  }
}
