package Rs

import chisel3._
import chisel3.util._

class ErrEvalXlInv(c: Config) extends Module {
  val io = IO(new Bundle {
    val errEvalIf = Input(Valid(new vecFfsIf(c.T_LEN+1, c.SYMB_WIDTH)))
    val XlInvIf = Input(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val errEvalXlInvIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
  })

  val shiftVec = Module(new ShiftBundleMod(UInt(c.SYMB_WIDTH.W), c.T_LEN, c.forneyEEXlInvTermsPerCycle))
  val stage = for(i <- 0 until c.forneyEEXlInvTermsPerCycle) yield Module(new ErrEvalXlInvStage(c))
  val stageOut = Wire(Vec(c.forneyEEXlInvTermsPerCycle, Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W))))

  // Shift XlInvIf
  shiftVec.io.vecIn.bits  := io.XlInvIf.bits.vec
  shiftVec.io.vecIn.valid := io.errEvalIf.valid

  for(i <- 0 until c.forneyEEXlInvTermsPerCycle) {
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

  val accumMat = Module(new AccumMat(c.SYMB_WIDTH, c.T_LEN+1, c.forneyEEXlInvTermsPerCycle, c.forneyEEXlInvShiftLatency, c.T_LEN))

  accumMat.io.vecIn := stageOut

  //val sel = io.XlInvIf.bits.ffs << 1
  val sel = io.errEvalIf.bits.ffs << 1
  val errEvalXlInvVec = Reg(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
  val errEvalXlInvVld = RegNext(next=lastQ, 0.U)
  val errEvalXlInvFfs = Reg(UInt(c.T_LEN.W))

  when(lastQ) {
    errEvalXlInvVec := Mux1H(sel, accumMat.io.matTOut)
    errEvalXlInvFfs := io.errEvalIf.bits.ffs
  }

  val errEvalFfs = io.errEvalIf.bits.ffs
  dontTouch(errEvalFfs)

  ///////////////////////////////////
  // Output signal
  ///////////////////////////////////

  io.errEvalXlInvIf.bits.vec := errEvalXlInvVec
  io.errEvalXlInvIf.valid := errEvalXlInvVld
  // TODO : What FFS to connect ?! 
  io.errEvalXlInvIf.bits.ffs := errEvalXlInvFfs

}

class ErrEvalXlInvStage(c: Config) extends Module {
  val io = IO(new Bundle {
    val vecIn = Input(Valid(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W))))
    val XlInvSymb = Input(UInt(c.SYMB_WIDTH.W))
    val vecOut = Output(Valid(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W))))
  })

  val qStage = Reg(Vec(c.forneyEEXlInvQStages, (Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))))
  val comboStage = Wire(Vec(c.forneyEEXlInvQStages, (Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))))

  val qXlInvStage = Reg(Vec(c.forneyEEXlInvQStages, UInt(c.SYMB_WIDTH.W)))

  io.vecOut.bits := qStage(c.forneyEEXlInvQStages-1)
  io.vecOut.valid := ShiftRegister(io.vecIn.valid, c.forneyEEXlInvQStages, false.B, true.B)

  for(i <- 0 until c.forneyEEXlInvQStages) {
    val start_indx = 1+i*c.forneyEEXlInvComboLen
    val stop_indx = 1+c.forneyEEXlInvComboLen+i*c.forneyEEXlInvComboLen
    val initStage = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
    val initXlInv = Wire(UInt(c.SYMB_WIDTH.W))

    if(i == 0) {
      initStage := io.vecIn.bits
      initXlInv := io.XlInvSymb
    }
    else {
      initStage := qStage(i-1)
      initXlInv := qXlInvStage(i-1)
    }
    qXlInvStage(i) := initXlInv

    for(k <- 0 until c.T_LEN+1) {
      if(k < start_indx)
        comboStage(i)(k) := initStage(k)
      else if(k < stop_indx)
        comboStage(i)(k) := c.gfMult(comboStage(i)(k-1), initXlInv) ^ initStage(k)
      else
        comboStage(i)(k) := initStage(k)
    }
    qStage(i) := comboStage(i)
  }

}

