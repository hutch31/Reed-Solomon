package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsBlockRecovery extends Module with GfParams {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(axisWidth)))
    val mAxisIf = Output(Valid(new axisIf(axisWidth)))
  })

  // Instance RsDecoder
  val rsDecoder = Module(new RsDecoder)
  rsDecoder.io.sAxisIf <> io.sAxisIf

  // Queue is used to store incomming messages

  class sQueueBundle(width: Int) extends Bundle {
    val tdata = Vec(width, UInt(symbWidth.W))
    val tlast = Bool()
  }
  val sQueue = Module(new Queue(new sQueueBundle(axisWidth), 3*msgDuration))

  sQueue.io.enq.valid := io.sAxisIf.valid
  sQueue.io.enq.bits.tdata := io.sAxisIf.bits.tdata
  sQueue.io.enq.bits.tlast := io.sAxisIf.bits.tlast

  val cntr = RegInit(UInt((log2Ceil(axisWidth * msgDuration)+1).W), 0.U)
  //val cntrNxt = Wire(UInt(log2Ceil(axisWidth * msgDuration)+1).W)

  val mReady = RegInit(Bool(),false.B)
  sQueue.io.deq.ready := mReady

  // Counter controlls READ operation
  when(rsDecoder.io.errValIf.valid) {
    cntr := cntr + axisWidth.U    
    mReady := true.B
  }.elsewhen(cntr.orR){
    when(cntr === (axisWidth * (msgDuration+1)).U) {
      cntr := 0.U
      mReady := false.B
    }.otherwise{
      cntr := cntr + axisWidth.U
    }
  }

  // Shift position value is less than current cntr value
  val shiftEnableVec = Wire(Vec(axisWidth, UInt(1.W)))
  val shiftVal = shiftEnableVec.reduce(_+_)

  val errPosVecRev = VecInit(rsDecoder.io.errPosIf.bits.vec.reverse)
  val errValVecRev = VecInit(rsDecoder.io.errValIf.bits.vec.reverse)

  val errPosAxis = Wire(Vec(axisWidth, UInt(log2Ceil(axisWidth).W)))
  val errPosVec = Reg(Vec(tLen, UInt(symbWidth.W)))
  val errValVec = Reg(Vec(tLen, UInt(symbWidth.W)))
  val errPosSel = Reg(UInt(tLen.W))

  val ffsCountOnes = symbWidth.U - PopCount(rsDecoder.io.errPosIf.bits.ffs)
  
  when(rsDecoder.io.errPosIf.valid) {
    errPosVec := (errPosVecRev.asUInt >> (symbWidth.U * ffsCountOnes)).asTypeOf(Vec(tLen, UInt(symbWidth.W)))
    errValVec := (errValVecRev.asUInt >> (symbWidth.U * ffsCountOnes)).asTypeOf(Vec(tLen, UInt(symbWidth.W)))
    errPosSel := Reverse(rsDecoder.io.errPosIf.bits.ffs) >> ffsCountOnes    
  }.elsewhen(shiftEnableVec.reduce(_|_) === 1.U){
    errPosVec := (errPosVec.asUInt >> (symbWidth.U * shiftVal)).asTypeOf(Vec(tLen, UInt(symbWidth.W)))
    errValVec := (errValVec.asUInt >> (symbWidth.U * shiftVal)).asTypeOf(Vec(tLen, UInt(symbWidth.W)))
    errPosSel := errPosSel >> shiftVal
  }

  // Shift enable
  for(i <- 0 until axisWidth) {
    errPosAxis(i) := errPosVec(i) % axisWidth.U
    when(errPosVec(i) < cntr) {
      shiftEnableVec(i) := errPosSel(i)
    }.otherwise{
      shiftEnableVec(i) := 0.U
    }
  }

  val mTdata = Wire(Vec(axisWidth, UInt(symbWidth.W)))
  val mTkeep = Wire(UInt(axisWidth.W))

  dontTouch(mTdata)

  mTdata := sQueue.io.deq.bits.tdata  

  when(sQueue.io.deq.bits.tlast){
    mTkeep := Fill(nLen % axisWidth, 1.U)
  }.otherwise{
    mTkeep := Fill(axisWidth, 1.U)
  }
  for(i <- 0 until axisWidth) {
    when(shiftEnableVec(i) === 1.U){
      mTdata(errPosAxis(i)) := sQueue.io.deq.bits.tdata(errPosAxis(i)) ^ errValVec(i)
    }
  }

  io.mAxisIf.valid := mReady
  io.mAxisIf.bits.tdata := mTdata
  io.mAxisIf.bits.tlast := sQueue.io.deq.bits.tlast
  io.mAxisIf.bits.tkeep := mTkeep

}

// runMain Rs.GenRsBlockRecovery
object GenRsBlockRecovery extends App {
  ChiselStage.emitSystemVerilogFile(new RsBlockRecovery(), Array())
}

