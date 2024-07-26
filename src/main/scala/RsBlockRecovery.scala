package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsBlockRecovery(config: Config) extends Module with GfParams {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(config.BUS_WIDTH)))
    val mAxisIf = Output(Valid(new axisIf(config.BUS_WIDTH)))
  })

  val msgNum = 3

  // Instance RsDecoder
  val rsDecoder = Module(new RsDecoder(config))
  rsDecoder.io.sAxisIf <> io.sAxisIf
  val goodMsg = ~rsDecoder.io.msgCorrupted & rsDecoder.io.syndValid
  val startMsg = Wire(Bool())

  // store error positions and values in the queues.
  // When the message is not corrupted make queueErrPos.io.enq.bits.ffs = 0
  val queueErrPos = Module(new Queue(new vecFfsIf(tLen), msgNum))
  queueErrPos.io.enq.valid := rsDecoder.io.errPosIf.valid
  queueErrPos.io.enq.bits.vec := rsDecoder.io.errPosIf.bits.vec
  queueErrPos.io.enq.bits.ffs := rsDecoder.io.errPosIf.bits.ffs
  queueErrPos.io.deq.ready := startMsg

  val queueErrVal = Module(new Queue(Vec(tLen, UInt(config.SYMB_WIDTH.W)), msgNum))
  queueErrVal.io.enq.valid := rsDecoder.io.errValIf.valid
  queueErrVal.io.enq.bits := rsDecoder.io.errValIf.bits.vec
  queueErrVal.io.deq.ready := startMsg

  // sQueue is used to store incomming messages
  class sQueueBundle(width: Int) extends Bundle {
    val tdata = Vec(width, UInt(config.SYMB_WIDTH.W))
    val tlast = Bool()
  }
  val sQueue = Module(new Queue(new sQueueBundle(axisWidth), msgNum*msgDuration))

  sQueue.io.enq.valid := io.sAxisIf.valid
  sQueue.io.enq.bits.tdata := io.sAxisIf.bits.tdata
  sQueue.io.enq.bits.tlast := io.sAxisIf.bits.tlast

  
  val correctMsg = startMsg & queueErrPos.io.deq.bits.ffs.orR

  // dscQueue is used to store message description. Is it corrupted ot not 

  val cntr = RegInit(UInt((log2Ceil(axisWidth * msgDuration)+1).W), 0.U)
  val mReady = RegInit(Bool(),false.B)
  sQueue.io.deq.ready := mReady

  // Counter controlls READ operation from sQueue
  when(startMsg) {
    cntr := cntr + axisWidth.U
    mReady := true.B
  }.elsewhen(mReady){
    when(sQueue.io.deq.bits.tlast) {
      cntr := 0.U
      mReady := false.B
    }.otherwise{
      cntr := cntr + axisWidth.U
    }
  }

  // Read out the message when queueErrVal is not empty
  // and there is no reading operation
  when(~mReady & queueErrVal.io.deq.valid) {
    startMsg := true.B
  }.otherwise{
    startMsg := false.B
  }

  // Shift position value is less than current cntr value
  val shiftEnableVec = Wire(Vec(axisWidth, UInt(1.W)))
  val shiftVal = shiftEnableVec.reduce(_+&_)
  dontTouch(shiftVal)

  val errPosVecRev = VecInit(queueErrPos.io.deq.bits.vec.reverse)
  val errValVecRev = VecInit(queueErrVal.io.deq.bits.reverse)

  val errPosAxis = Wire(Vec(axisWidth, UInt(log2Ceil(axisWidth).W)))
  val errPosVec = Reg(Vec(tLen, UInt(config.SYMB_WIDTH.W)))
  val errValVec = Reg(Vec(tLen, UInt(config.SYMB_WIDTH.W)))
  val errPosSel = Reg(UInt(tLen.W))

  val ffsCountOnes = config.SYMB_WIDTH.U - PopCount(queueErrPos.io.deq.bits.ffs)
  
  when(correctMsg) {
    errPosVec := (errPosVecRev.asUInt >> (config.SYMB_WIDTH.U * ffsCountOnes)).asTypeOf(Vec(tLen, UInt(config.SYMB_WIDTH.W)))
    errValVec := (errValVecRev.asUInt >> (config.SYMB_WIDTH.U * ffsCountOnes)).asTypeOf(Vec(tLen, UInt(config.SYMB_WIDTH.W)))
    errPosSel := Reverse(rsDecoder.io.errPosIf.bits.ffs) >> ffsCountOnes    
  }.elsewhen(shiftEnableVec.reduce(_|_) === 1.U){
    errPosVec := (errPosVec.asUInt >> (config.SYMB_WIDTH.U * shiftVal)).asTypeOf(Vec(tLen, UInt(config.SYMB_WIDTH.W)))
    errValVec := (errValVec.asUInt >> (config.SYMB_WIDTH.U * shiftVal)).asTypeOf(Vec(tLen, UInt(config.SYMB_WIDTH.W)))
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

  val mTdata = Wire(Vec(axisWidth, UInt(config.SYMB_WIDTH.W)))
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
  val projectRoot = System.getProperty("project.root")
  val c = JsonReader.readConfig(projectRoot + "/rs.json")
  ChiselStage.emitSystemVerilogFile(new RsBlockRecovery(c), Array())
}

