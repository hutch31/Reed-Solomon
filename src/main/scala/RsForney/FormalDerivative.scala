package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

class FormalDerivative(c: Config) extends Module {
  val io = IO(new Bundle {
    val XlInvIf = Input(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val Xl = Input(Vec(c.T_LEN, (UInt(c.SYMB_WIDTH.W))))
    val formalDerIf    = Output(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
  })

  /////////////////////////////////
  // FD0 stage
  // mult(Xl[])
  /////////////////////////////////

  val XlInvShift = Reg(Vec(c.T_LEN, (UInt(c.SYMB_WIDTH.W))))
  val cntr = RegInit(UInt(log2Ceil(c.cntrStopLimitFd0).W), 0.U)

  // Load data into the shift register
  when(io.XlInvIf.valid) {
    XlInvShift := io.XlInvIf.bits.vec
  }.otherwise{
    // Rotate
    for(i <- 0 until c.T_LEN-c.numOfStagesFd0)
      XlInvShift(i) := XlInvShift(i+c.numOfStagesFd0)
  }

  val lastCycleFd0 = Wire(Bool())

  // cntr that controls pipe execution
  if(c.numOfStagesFd0 == c.T_LEN) {
    cntr := 0.U
    lastCycleFd0 := RegNext(next=io.XlInvIf.valid, init=false.B)
  } else {    
    val start_cntr = RegNext(next=io.XlInvIf.valid, init=false.B)
    when(start_cntr){
      cntr := c.numOfStagesFd0.U
    }.elsewhen(cntr === c.cntrEopFd0.U) {
      cntr := 0.U
    }.elsewhen(cntr =/= 0.U){
      cntr := cntr + c.numOfStagesFd0.U
    }
    // last cycle is used to capture the output value
    when (cntr === c.cntrEopFd0.U) {
      lastCycleFd0 := true.B
    }.otherwise{
      lastCycleFd0 := false.B
    }
  }

  val XlMultXlInv = Wire(Vec(c.numOfStagesFd0, (Vec(c.T_LEN-1, (UInt(c.SYMB_WIDTH.W))))))
  val stageEoPFd1 = Wire(Bool())

  val stageFd1 = for(i <- 0 until c.numOfStagesFd0) yield Module(new FormalDerivativeStage1(c))
  val stageFd0 = for(i <- 0 until c.numOfStagesFd0) yield Module(new DeleteItem(c.T_LEN, c.SYMB_WIDTH))
  val stageOut = Wire(Vec(c.numOfStagesFd0, (Vec(c.T_LEN-1, (UInt(c.SYMB_WIDTH.W))))))

  for(i <- 0 until c.numOfStagesFd0) {
    stageOut(i) := stageFd0(i).io.out
    stageFd0(i).io.in := io.Xl
    stageFd0(i).io.sel := cntr+i.U
    for(j <- 0 until c.T_LEN-1) {
      XlMultXlInv(i)(j) := c.gfMult(stageOut(i)(j), XlInvShift(i)) ^ 1.U
    }    
  }

  /////////////////////////////////
  // FD1 stage
  /////////////////////////////////

  stageEoPFd1 := ShiftRegister(lastCycleFd0, c.numOfQStagesFd1+1, false.B, true.B)

  for(i <- 0 until c.numOfStagesFd0) {    
    stageFd1(i).io.in := XlMultXlInv(i)
  }    

  // Pipelining FD1
  val pipeFd1Q = Reg(Vec(c.numOfCyclesFd0, (Vec(c.numOfStagesFd0, (Vec(c.T_LEN-1, (UInt(c.SYMB_WIDTH.W))))))))
  val pipeFd1VldQ = RegNext(next=stageEoPFd1, init=false.B)
  val pipeFd1 = Wire(Vec(c.T_LEN, (Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))))

  for(i <- 0 until c.numOfCyclesFd0) {
    for(k <- 0 until c.numOfStagesFd0) {
      if(i == 0 )
        pipeFd1Q(c.numOfCyclesFd0-1)(k) := stageFd1(k).io.out
      else
        pipeFd1Q(c.numOfCyclesFd0-1-i)(k) := pipeFd1Q(c.numOfCyclesFd0-i)(k)
      if(i*c.numOfStagesFd0+k < c.T_LEN)
        pipeFd1(i*c.numOfStagesFd0+k) := pipeFd1Q(i)(k)
    }    
  }

  // Formal derivative
  val formalDerArray = Wire(Vec(c.T_LEN, (Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))))

  for(m <- 0 until c.T_LEN) {
    for(n <- 0 until c.T_LEN) {
      if(m == 0)
        formalDerArray(m)(n) := 1.U
      else
        formalDerArray(m)(n) := pipeFd1(n)(m-1)
    }
  }

  val formalDer = Reg(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))

  when(pipeFd1VldQ) {
    formalDer := Mux1H(io.XlInvIf.bits.ffs, formalDerArray)
  }

  io.formalDerIf := formalDer

}

class FormalDerivativeStage1(c: Config) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))
    val out = Output(Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))
  })

  /////////////////////////////////
  // FD1 stage
  /////////////////////////////////

  val qStage = Reg(Vec(c.numOfQStagesFd1+1, (Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))))
  val comboStage = Wire(Vec(c.numOfQStagesFd1, (Vec(c.T_LEN-1, UInt(c.SYMB_WIDTH.W)))))

  qStage(0) := io.in
  io.out := qStage(c.numOfQStagesFd1)
  
  for(i <- 0 until c.numOfQStagesFd1){
    val start_indx = 1+i*c.numOfComboLenFd1
    val stop_indx = 1+c.numOfComboLenFd1+i*c.numOfComboLenFd1
    for(k <- 0 until c.T_LEN-1) {
      if(k < start_indx)
        comboStage(i)(k) := qStage(i)(k)
      else if(k < stop_indx)
        comboStage(i)(k) := c.gfMult(comboStage(i)(k-1), qStage(i)(k))
      else
        comboStage(i)(k) := qStage(i)(k)
    }
    qStage(i+1) := comboStage(i)
  }
}


