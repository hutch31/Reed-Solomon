package Rs

import chisel3._
import chisel3.util._

class ErrEvalXlInv extends Module with GfParams {
  val io = IO(new Bundle {
    val errEvalIf = Input(Valid(new vecFfsIf(tLen+1)))
    val XlInvIf = Input(Valid(new vecFfsIf(tLen)))
    val errEvalXlInvIf = Output(Valid(new vecFfsIf(tLen)))
  })

  val shiftVec = Module(new ShiftBundleMod(UInt(symbWidth.W), tLen, numOfQStagesEe0))
  val stage = for(i <- 0 until numOfQStagesEe0) yield Module(new ErrEvalXlInvStage)
  val stageOut = Wire(Vec(numOfQStagesEe0, Vec(tLen+1, UInt(symbWidth.W))))

  // Shift XlInvIf
  shiftVec.io.vecIn.bits  := io.XlInvIf.bits.vec
  shiftVec.io.vecIn.valid := io.errEvalIf.valid

  for(i <- 0 until numOfQStagesEe0) {
    stage(i).io.vecIn.bits := io.errEvalIf.bits.vec
    stage(i).io.vecIn.valid := shiftVec.io.lastOut
    stage(i).io.XlInvSymb := shiftVec.io.vecOut.bits(i)
    stageOut(i) := stage(i).io.vecOut.bits
  }

  ///////////////////////////////////
  // Accum matrix
  ///////////////////////////////////

  // The matrix fully loaded into accumMat when lastQ is asserted
  val lastQ = RegNext(stage(0).io.vecOut.valid)

  val numOfCyclesEe0 = math.ceil(tLen/numOfStagesEe0.toDouble).toInt
  val accumMat = Module(new AccumMat(symbWidth, tLen+1, numOfQStagesEe0, numOfCyclesEe0, tLen))

  accumMat.io.vecIn := stageOut

  // Capture errEvalXlInvVec value
  // TODO : what FFS to use. Do I need to pipeline it ?! Should ffs be pipelined ?
  val sel = io.XlInvIf.bits.ffs << 1
  val errEvalXlInvVec = Reg(Vec(tLen, UInt(symbWidth.W)))
  val errEvalXlInvVld = RegNext(next=lastQ, 0.U)
  val errEvalXlInvFfs = Reg(UInt(tLen.W))

  when(lastQ) {
    errEvalXlInvVec := Mux1H(sel, accumMat.io.matTOut)
    errEvalXlInvFfs := io.XlInvIf.bits.ffs // TODO: connect to the io.errEvalIf.bits.ffs
  }

  ///////////////////////////////////
  // Output signal
  ///////////////////////////////////

  io.errEvalXlInvIf.bits.vec := errEvalXlInvVec
  io.errEvalXlInvIf.valid := errEvalXlInvVld
  // TODO : What FFS to connect ?! 
  io.errEvalXlInvIf.bits.ffs := errEvalXlInvFfs

}

class ErrEvalXlInvStage extends Module with GfParams {
  val io = IO(new Bundle {
    val vecIn = Input(Valid(Vec(tLen+1, UInt(symbWidth.W))))
    val XlInvSymb = Input(UInt(symbWidth.W))
    val vecOut = Output(Valid(Vec(tLen+1, UInt(symbWidth.W))))
  })

  val qStage = Reg(Vec(numOfQStagesEe0, (Vec(tLen+1, UInt(symbWidth.W)))))
  val comboStage = Wire(Vec(numOfQStagesEe0, (Vec(tLen+1, UInt(symbWidth.W)))))

  val qXlInvStage = Reg(Vec(numOfQStagesEe0, UInt(symbWidth.W)))

  io.vecOut.bits := qStage(numOfQStagesEe0-1)
  io.vecOut.valid := ShiftRegister(io.vecIn.valid, numOfQStagesEe0, false.B, true.B)

  for(i <- 0 until numOfQStagesEe0) {
    val start_indx = 1+i*numOfComboLenEe0
    val stop_indx = 1+numOfComboLenEe0+i*numOfComboLenEe0
    val initStage = Wire(Vec(tLen+1, UInt(symbWidth.W)))
    val initXlInv = Wire(UInt(symbWidth.W))

    if(i == 0) {
      initStage := io.vecIn.bits
      initXlInv := io.XlInvSymb
    }
    else {
      initStage := qStage(i-1)
      initXlInv := qXlInvStage(i-1)
    }
    qXlInvStage(i) := initXlInv

    for(k <- 0 until tLen+1) {
      if(k < start_indx)
        comboStage(i)(k) := initStage(k)
      else if(k < stop_indx)
        comboStage(i)(k) := gfMult(comboStage(i)(k-1), initXlInv) ^ initStage(k)
      else
        comboStage(i)(k) := initStage(k)
    }
    qStage(i) := comboStage(i)
  }

}

