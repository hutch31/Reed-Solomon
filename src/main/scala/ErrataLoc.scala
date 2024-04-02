package Rs

import chisel3._
import chisel3.util._

class ErrataLoc extends Module with GfParams {
  val io = IO(new Bundle {
    val errPosCoefIf = Input(Valid(new vecFfsIf(tLen)))
    val errataLocIf = Output(Valid(new vecFfsIf(tLen+1)))
  })

  // Modules instances
  val stage = for(i <- 0 until numOfErrataLocStages) yield Module(new ErrataLocatorStage())

  // Slice valid bit that goes into comb stage(s)
  val errataLoc = Reg(Vec(tLen+1, UInt(symbWidth.W)))
  val stageOut = Wire(Vec(numOfErrataLocStages, (Vec(tLen+1, UInt(symbWidth.W)))))

  ///////////////////////////
  // Shift vec and ffs
  ///////////////////////////

  val shiftMod = Module(new ShiftBundleMod(new ShiftUnit, tLen, numOfErrataLocStages))

  class ShiftUnit extends Bundle {
    val symb = UInt(symbWidth.W)
    val ffs = Bool()
  }

  // Map inputs
  for(i <- 0 until tLen) {
    shiftMod.io.vecIn.bits(i).ffs := io.errPosCoefIf.bits.ffs.asTypeOf(Vec(tLen, Bool()))(i)
    shiftMod.io.vecIn.bits(i).symb := io.errPosCoefIf.bits.vec(i)
  }

  // Map outputs
  val errPosVldStage = shiftMod.io.vecOut.bits.map(_.ffs)
  val coefPositionShift = shiftMod.io.vecOut.bits.map(_.symb)

  val errataLocVld = RegInit(Bool(), 0.U)

  when(shiftMod.io.vecOut.valid) {
    errataLocVld := errPosVldStage.reduce(_ || _)
  }
  
  ///////////////////////////
  // Capture errataLoc value
  //
  // Output value of errata locator is stored in errataLoc register
  // it should be assigned to 1*x^0 + 0*x^1 + ... + 0*x^tLen when
  // errPosCoefIf.valid asserted.
  ///////////////////////////

  
  when (io.errPosCoefIf.valid) {
    for(i <- 0 until tLen+1) {
      if(i == 0)
        errataLoc(i) := 1.U
      else
        errataLoc(i) := 0.U
    }
  }.otherwise {
    // Capture errataLoc value
    when(errPosVldStage.reduce(_ || _) === 0.U) {
      errataLoc := stage(numOfErrataLocStages-1).io.errataLoc
    }.otherwise {      
      if(numOfErrataLocStages == 1)
        errataLoc := stage(numOfErrataLocStages-1).io.errataLoc
      else
        errataLoc := Mux1H(errPosVldStage, stageOut)
    }
  }

  // TODO : coefPositionShift 
  stage(0).io.errataLocPrev := errataLoc
  stage(0).io.coefPosition := coefPositionShift(0)
  stageOut(0) := stage(0).io.errataLoc

  if(numOfErrataLocStages > 1) {
    for(i <- 1 until numOfErrataLocStages) {
      stage(i).io.errataLocPrev := stage(i-1).io.errataLoc
      stage(i).io.coefPosition := coefPositionShift(i)
      stageOut(i) := stage(i).io.errataLoc
    }
  }

  // Capture FFS
  val ffsQ = Reg(UInt(tLen.W))

  when(errPosVldStage.reduce(_ || _) === 1.U) {
    ffsQ := io.errPosCoefIf.bits.ffs
  }

  // Output assignment
  io.errataLocIf.valid := errataLocVld
  io.errataLocIf.bits.vec := errataLoc
  io.errataLocIf.bits.ffs := ffsQ
  
}

class ErrataLocatorStage extends Module with GfParams{
  val io = IO(new Bundle {
    val errataLocPrev = Input(Vec(tLen+1, UInt(symbWidth.W)))
    val coefPosition = Input(UInt(symbWidth.W))
    val errataLoc = Output(Vec(tLen+1, UInt(symbWidth.W)))
  })

  val coefPositionPower = Wire(UInt(symbWidth.W))

  coefPositionPower := powerFirstRoot(io.coefPosition)

  for(symbIndx <- 0 until tLen+1) {
    if(symbIndx == 0)
      io.errataLoc(symbIndx) := 1.U
    else if(symbIndx == tLen)
      io.errataLoc(symbIndx) := gfMult(io.errataLocPrev(symbIndx-1), coefPositionPower)
    else
      io.errataLoc(symbIndx) := gfMult(io.errataLocPrev(symbIndx-1), coefPositionPower) ^ io.errataLocPrev(symbIndx)
  }
}
