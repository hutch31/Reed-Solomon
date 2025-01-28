package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

// TODO: use shiftUni instead o fshift reg
// TODO: use AccumMat output matTOut instead of transposed formalDerArray

class FormalDerivative(c: Config) extends Module {
  val io = IO(new Bundle {
    val XlInvIf = Input(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val Xl = Input(Vec(c.T_LEN, (UInt(c.SYMB_WIDTH.W))))
    // TODO: do we need valid signal here ?!
    val formalDerIf    = Output(Valid(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W))))
  })

  /////////////////////////////////
  // FD0 stage
  // mult(Xl[])
  /////////////////////////////////

  val XlInvShift = Reg(Vec(c.T_LEN, (UInt(c.SYMB_WIDTH.W))))

  // Load data into the shift register
  when(io.XlInvIf.valid) {
    XlInvShift := io.XlInvIf.bits.vec
  }.otherwise{
    // Rotate
    for(i <- 0 until c.T_LEN-c.forneyFdTermsPerCycle)
      XlInvShift(i) := XlInvShift(i+c.forneyFdTermsPerCycle)
  }

  val lastCycleFd0 = Wire(Bool())

  // cntr controls pipe execution
  val cntr = RegInit(UInt(log2Ceil(c.forneyFdStopLimit).W), 0.U)

  // If there is no pipeline
  if(c.forneyFdTermsPerCycle == c.T_LEN) {
    cntr := 0.U
    lastCycleFd0 := RegNext(next=io.XlInvIf.valid, init=false.B)
  } else {
    val start_cntr = RegNext(next=io.XlInvIf.valid, init=false.B)
    when(start_cntr){
      cntr := c.forneyFdTermsPerCycle.U
    }.elsewhen(cntr === c.forneyFdEop.U) {
      cntr := 0.U
    }.elsewhen(cntr =/= 0.U){
      cntr := cntr + c.forneyFdTermsPerCycle.U
    }
    // last cycle is used to capture the output value
    when (cntr === c.forneyFdEop.U) {
      lastCycleFd0 := true.B
    }.otherwise{
      lastCycleFd0 := false.B
    }
  }

  val XlMultXlInv = Wire(Vec(c.forneyFdTermsPerCycle, (Vec(c.T_LEN-1, (UInt(c.SYMB_WIDTH.W))))))

  val deleteItem = for(i <- 0 until c.forneyFdTermsPerCycle) yield Module(new DeleteItem(c.T_LEN, c.SYMB_WIDTH))

  for(i <- 0 until c.forneyFdTermsPerCycle) {
    deleteItem(i).io.in := io.Xl
    deleteItem(i).io.sel := cntr+i.U
    for(j <- 0 until c.T_LEN-1) {
      XlMultXlInv(i)(j) := c.gfMult(deleteItem(i).io.out(j), XlInvShift(i)) ^ 1.U
    }    
  }

  // Capture result into the registers
  val XlMultXlInvQ = RegNext(XlMultXlInv)
  val XlmultxlinvLastQ = RegNext(lastCycleFd0, init=false.B)
  val XlmultxlinvFfsQ = RegEnable(io.XlInvIf.bits.ffs, lastCycleFd0)

  /////////////////////////////////
  // FD stage
  /////////////////////////////////

  val stageFd = for(i <- 0 until c.forneyFdTermsPerCycle) yield Module(new FormalDerivativeStage(c))
  val stageFdLast = ShiftRegister(XlmultxlinvLastQ, c.forneyFdQStages, false.B, true.B)
  val stageFdFfs = ShiftRegister(XlmultxlinvFfsQ, c.forneyFdQStages, 0.U, true.B)

  for(i <- 0 until c.forneyFdTermsPerCycle) {    
    stageFd(i).io.in := XlMultXlInvQ(i)
  }

  /////////////////////////////////
  // Accumulate FDstage output
  /////////////////////////////////

  val accumMatLastQ = RegNext(stageFdLast, init=false.B)
  val accumMatFfsQ = RegNext(stageFdFfs, init=0.U)
  val accumMat = Module(new AccumMat(c.SYMB_WIDTH, c.T_LEN-1, c.forneyFdTermsPerCycle, c.forneyFdShiftLatency , c.T_LEN))
  accumMat.io.vecIn := VecInit(stageFd.map(_.io.out))

  // Formal derivative
  val formalDerArray = Wire(Vec(c.T_LEN, (Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))))

  for(m <- 0 until c.T_LEN) {
    for(n <- 0 until c.T_LEN) {
      if(m == 0)
        formalDerArray(m)(n) := 1.U
      else
        formalDerArray(m)(n) := accumMat.io.matOut(n)(m-1)
    }
  }

  val formalDer = RegEnable(Mux1H(accumMatFfsQ, formalDerArray), accumMatLastQ)

  io.formalDerIf.bits := formalDer
  io.formalDerIf.valid := accumMatLastQ

  /////////////////
  // Assert not ready
  /////////////////
  val notReadyAssrt = Module(new NotReadyAssrt())
  notReadyAssrt.io.start := io.XlInvIf.valid
  notReadyAssrt.io.stop := lastCycleFd0

}

class FormalDerivativeStage(c: Config) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))
    val out = Output(Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))
  })

  val qStage = Reg(Vec(c.forneyFdQStages, (Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))))
  val comboStage = Wire(Vec(c.forneyFdQStages, (Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))))

  io.out := qStage(c.forneyFdQStages-1)

  for(i <- 0 until c.forneyFdQStages){

    val initStage = Wire(Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))

    if(i == 0) {
      initStage := io.in
    } else {
      initStage := qStage(i-1)
    }

    // Indexes determine positions where logic
    // should be inserted.

    val start_indx = 1+i*c.forneyFdComboLen
    val stop_indx = 1+c.forneyFdComboLen+i*c.forneyFdComboLen

    for(k <- 0 until c.T_LEN-1) {
      if(k < start_indx)
        comboStage(i)(k) := initStage(k)
      else if(k < stop_indx)
        comboStage(i)(k) := c.gfMult(comboStage(i)(k-1), initStage(k))
      else
        comboStage(i)(k) := initStage(k)
    }
    qStage(i) := comboStage(i)
  }
}


