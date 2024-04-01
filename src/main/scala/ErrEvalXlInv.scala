package Rs

import chisel3._
import chisel3.util._

class ErrEvalXlInv extends Module with GfParams {
  val io = IO(new Bundle {
    val errEvalIf = Input(Valid(Vec(tLen+1, UInt(symbWidth.W))))
    val XlInvIf = Input(Valid(new vecFfsIf(tLen)))
    val errEvalXlInvIf = Output(Valid(new vecFfsIf(tLen)))
  })

  val shiftVec = Module(new ShiftVec(tLen, symbWidth, numOfQStagesEe0))
  val stage = for(i <- 0 until numOfQStagesEe0) yield Module(new ErrEvalXlInvStage)
  val stageOut = Wire(Vec(numOfQStagesEe0, Vec(tLen+1, UInt(symbWidth.W))))

  // Shift XlInvIf
  shiftVec.io.vecIn.bits  := io.XlInvIf.bits.vec
  shiftVec.io.vecIn.valid := io.errEvalIf.valid

  for(i <- 0 until numOfQStagesEe0) {
    stage(i).io.vecIn.bits := io.errEvalIf.bits
    stage(i).io.vecIn.valid := shiftVec.io.vecOut.bits.last
    stage(i).io.XlInvSymbIn := shiftVec.io.vecOut.bits.vec(i)
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

  when(lastQ) {
    errEvalXlInvVec := Mux1H(sel, accumMat.io.matTOut)
  }

  ///////////////////////////////////
  // Output signal
  ///////////////////////////////////

  io.errEvalXlInvIf.bits.vec := errEvalXlInvVec
  io.errEvalXlInvIf.valid := errEvalXlInvVld
  // TODO : DO we need ffs ?!
  io.errEvalXlInvIf.bits.ffs := 0.U

}

class ErrEvalXlInvStage extends Module with GfParams {
  val io = IO(new Bundle {
    val vecIn = Input(Valid(Vec(tLen+1, UInt(symbWidth.W))))
    val XlInvSymbIn = Input(UInt(symbWidth.W))
    val vecOut = Output(Valid(Vec(tLen+1, UInt(symbWidth.W))))
  })

  val qStage = Reg(Vec(numOfQStagesEe0, (Vec(tLen+1, UInt(symbWidth.W)))))
  val comboStage = Wire(Vec(numOfQStagesEe0, (Vec(tLen+1, UInt(symbWidth.W)))))

  val qXlInvStage = Reg(Vec(numOfQStagesEe0, UInt(symbWidth.W)))

  io.vecOut.bits := qStage(numOfQStagesEe0-1)
  io.vecOut.valid := ShiftRegister(io.vecIn.valid, numOfQStagesEe0, false.B, true.B)
  // numOfComboLenEe0 = 3
  // numOfQStagesEe0 = 3

  for(i <- 0 until numOfQStagesEe0) {
    val start_indx = 1+i*numOfComboLenEe0
    val stop_indx = 1+numOfComboLenEe0+i*numOfComboLenEe0
    val initStage = Wire(Vec(tLen+1, UInt(symbWidth.W)))
    val initXlInv = Wire(UInt(symbWidth.W))

    if(i == 0) {
      initStage := io.vecIn.bits
      initXlInv := io.XlInvSymbIn
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

