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

  val sQueue = Module(new Queue(Vec(axisWidth, UInt(symbWidth.W)), msgDuration))

  sQueue.io.enq.valid := io.sAxisIf.valid
  sQueue.io.enq.bits := io.sAxisIf.bits.tdata  

  val cntr = RegInit(UInt((log2Ceil(axisWidth * msgDuration)+1).W), 0.U)

  val mValid = RegInit(Bool(),false.B)
  val mReady = RegInit(Bool(),false.B)

  // Counter controlls READ operation
  when(rsDecoder.io.errPosIf.valid) {
    cntr := axisWidth.U
    mValid := true.B
    mReady := true.B
  }.otherwise{
    when(cntr === (axisWidth * msgDuration).U) {
      cntr := cntr
      mValid := false.B
      mReady := false.B
    }.otherwise{
      cntr := cntr + axisWidth.U
    }
  }

  sQueue.io.deq.ready := mReady

  // Shift position value is less than current cntr value
  val shiftEnableVec = Wire(Vec(axisWidth, UInt(1.W)))
  val shiftVal = shiftEnableVec.reduce(_+_)

  val errPosAxis = Reg(Vec(axisWidth, UInt(log2Ceil(axisWidth).W)))
  val errPosVec = Reg(Vec(tLen, UInt(symbWidth.W)))
  val errValVec = Reg(Vec(tLen, UInt(symbWidth.W)))
  val errPosSel = Reg(UInt(tLen.W))

  when(rsDecoder.io.errPosIf.valid) {
    errPosVec := rsDecoder.io.errPosIf.bits.vec
    errValVec := rsDecoder.io.errValIf.bits.vec
    errPosSel := rsDecoder.io.errPosIf.bits.ffs
  }.elsewhen(shiftEnableVec.reduce(_|_) === 1.U){
    errPosVec := (errPosVec.asUInt >> (symbWidth.U * shiftVal)).asTypeOf(Vec(tLen, UInt(symbWidth.W)))
    errValVec := (errValVec.asUInt >> (symbWidth.U * shiftVal)).asTypeOf(Vec(tLen, UInt(symbWidth.W)))
    errPosSel := errPosSel >> shiftVal
  }

  // Shift enable 
  for(i <- 0 until axisWidth) {
    errPosAxis(i) := errPosVec(i) % axisWidth.U
    when(errPosVec(i) < cntr) {
      shiftEnableVec(i) := 1.U
    }.otherwise{
      shiftEnableVec(i) := 0.U
    }
  }

  //
  val mTdata = Wire(Vec(axisWidth, UInt(symbWidth.W)))
  mTdata := sQueue.io.deq.bits
  for(i <- 0 until axisWidth) {
    when(shiftEnableVec(i) === 1.U){
      mTdata(errPosAxis(i)) := sQueue.io.deq.bits(i) ^ errValVec(i)
    }
  }

  io.mAxisIf.valid := mValid
  io.mAxisIf.bits.tdata := mTdata
  io.mAxisIf.bits.tlast := false.B
  io.mAxisIf.bits.tkeep := 0.U

}

// runMain Rs.GenRsBlockRecovery
object GenRsBlockRecovery extends App {
  ChiselStage.emitSystemVerilogFile(new RsBlockRecovery(), Array())
}

