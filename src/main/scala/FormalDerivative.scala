package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

class FormalDerivative extends Module with GfParams {
  val io = IO(new Bundle {
    val XlInvIf = Input(Valid(new vecFfsIf(tLen)))
    val Xl = Input(Vec(tLen, (UInt(symbWidth.W))))
    val formalDerIf    = Output(Vec(tLen, UInt(symbWidth.W)))
  })

  /////////////////////////////////
  // FD0 stage
  // mult(Xl[])
  /////////////////////////////////

  val XlInvShift = Reg(Vec(tLen, (UInt(symbWidth.W))))
  val cntr = RegInit(UInt(log2Ceil(cntrStopLimitFd0).W), 0.U)

  // Load data into the shift register
  when(io.XlInvIf.valid) {
    XlInvShift := io.XlInvIf.bits.vec
  }.otherwise{
    // Rotate
    for(i <- 0 until tLen-numOfStagesFd0)
      XlInvShift(i) := XlInvShift(i+numOfStagesFd0)
  }

  val lastCycleFd0 = Wire(Bool())

  // cntr that controls pipe execution
  if(numOfStagesFd0 == tLen) {
    cntr := 0.U
    lastCycleFd0 := RegNext(next=io.XlInvIf.valid, init=false.B)
  } else {    
    val start_cntr = RegNext(next=io.XlInvIf.valid, init=false.B)
    when(start_cntr){
      cntr := numOfStagesFd0.U
    }.elsewhen(cntr === cntrEopFd0.U) {
      cntr := 0.U
    }.elsewhen(cntr =/= 0.U){
      cntr := cntr + numOfStagesFd0.U
    }
    // last cycle is used to capture the output value
    when (cntr === cntrEopFd0.U) {
      lastCycleFd0 := true.B
    }.otherwise{
      lastCycleFd0 := false.B
    }
  }

  val XlMultXlInv = Wire(Vec(numOfStagesFd0, (Vec(tLen-1, (UInt(symbWidth.W))))))
  val stageEoPFd1 = Wire(Bool())

  val stageFd1 = for(i <- 0 until numOfStagesFd0) yield Module(new FormalDerivativeStage1)
  val stageFd0 = for(i <- 0 until numOfStagesFd0) yield Module(new DeleteItem(tLen, symbWidth))
  val stageOut = Wire(Vec(numOfStagesFd0, (Vec(tLen-1, (UInt(symbWidth.W))))))

  for(i <- 0 until numOfStagesFd0) {
    stageOut(i) := stageFd0(i).io.out
    stageFd0(i).io.in := io.Xl
    stageFd0(i).io.sel := cntr+i.U
    for(j <- 0 until tLen-1) {
      XlMultXlInv(i)(j) := gfMult(stageOut(i)(j), XlInvShift(i)) ^ 1.U
    }    
  }

  /////////////////////////////////
  // FD1 stage
  /////////////////////////////////

  stageEoPFd1 := ShiftRegister(lastCycleFd0, numOfQStagesFd1+1, false.B, true.B)

  for(i <- 0 until numOfStagesFd0) {    
    stageFd1(i).io.in := XlMultXlInv(i)
  }    

  // Pipelining FD1
  val pipeFd1Q = Reg(Vec(numOfCyclesFd0, (Vec(numOfStagesFd0, (Vec(tLen-1, (UInt(symbWidth.W))))))))
  val pipeFd1VldQ = RegNext(next=stageEoPFd1, init=false.B)
  val pipeFd1 = Wire(Vec(tLen, (Vec(tLen-1, UInt(symbWidth.W)))))

  for(i <- 0 until numOfCyclesFd0) {
    for(k <- 0 until numOfStagesFd0) {
      if(i == 0 )
        pipeFd1Q(numOfCyclesFd0-1)(k) := stageFd1(k).io.out
      else
        pipeFd1Q(numOfCyclesFd0-1-i)(k) := pipeFd1Q(numOfCyclesFd0-i)(k)
      if(i*numOfStagesFd0+k < tLen)
        pipeFd1(i*numOfStagesFd0+k) := pipeFd1Q(i)(k)
    }    
  }

  // Formal derivative
  val formalDerArray = Wire(Vec(tLen, (Vec(tLen, UInt(symbWidth.W)))))

  for(m <- 0 until tLen) {
    for(n <- 0 until tLen) {
      if(m == 0)
        formalDerArray(m)(n) := 1.U
      else
        formalDerArray(m)(n) := pipeFd1(n)(m-1)
    }
  }

  val formalDer = Reg(Vec(tLen, UInt(symbWidth.W)))

  when(pipeFd1VldQ) {
    formalDer := Mux1H(io.XlInvIf.bits.ffs, formalDerArray)
  }

  io.formalDerIf := formalDer

}

class FormalDerivativeStage1 extends Module with GfParams {
  val io = IO(new Bundle {
    val in = Input(Vec(tLen-1, UInt(symbWidth.W)))
    val out = Output(Vec(tLen-1, UInt(symbWidth.W)))
  })

  /////////////////////////////////
  // FD1 stage
  /////////////////////////////////

  val qStage = Reg(Vec(numOfQStagesFd1+1, (Vec(tLen-1, UInt(symbWidth.W)))))
  val comboStage = Wire(Vec(numOfQStagesFd1, (Vec(tLen-1, UInt(symbWidth.W)))))

  qStage(0) := io.in
  io.out := qStage(numOfQStagesFd1)
  
  for(i <- 0 until numOfQStagesFd1){
    val start_indx = 1+i*numOfComboLenFd1
    val stop_indx = 1+numOfComboLenFd1+i*numOfComboLenFd1
    for(k <- 0 until tLen-1) {
      if(k < start_indx)
        comboStage(i)(k) := qStage(i)(k)
      else if(k < stop_indx)
        comboStage(i)(k) := gfMult(comboStage(i)(k-1), qStage(i)(k))
      else
        comboStage(i)(k) := qStage(i)(k)
    }
    qStage(i+1) := comboStage(i)
  }
}


