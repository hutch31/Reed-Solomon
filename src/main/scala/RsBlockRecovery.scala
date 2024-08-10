package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsBlockRecovery(c: Config) extends Module {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(c.BUS_WIDTH, c.SYMB_WIDTH)))
    val mAxisIf = Output(Valid(new axisIf(c.BUS_WIDTH, c.SYMB_WIDTH)))
  })

  val msgNum = 3

  // Instance RsDecoder
  val rsDecoder = Module(new RsDecoder(c))
  rsDecoder.io.sAxisIf <> io.sAxisIf
  val goodMsg = ~rsDecoder.io.msgCorrupted & rsDecoder.io.syndValid
  val startMsg = Wire(Bool())

  // store error positions and values in the queues.
  // When the message is not corrupted make queueErrPos.io.enq.bits.ffs = 0
  val queueErrPos = Module(new Queue(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH), msgNum))
  queueErrPos.io.enq.valid := rsDecoder.io.errPosIf.valid
  queueErrPos.io.enq.bits.vec := rsDecoder.io.errPosIf.bits.vec
  queueErrPos.io.enq.bits.ffs := rsDecoder.io.errPosIf.bits.ffs
  queueErrPos.io.deq.ready := startMsg

  val queueErrVal = Module(new Queue(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)), msgNum))
  queueErrVal.io.enq.valid := rsDecoder.io.errValIf.valid
  queueErrVal.io.enq.bits := rsDecoder.io.errValIf.bits.vec
  queueErrVal.io.deq.ready := startMsg

  // sQueue is used to store incomming messages
  class sQueueBundle(width: Int) extends Bundle {
    val tdata = Vec(width, UInt(c.SYMB_WIDTH.W))
    val tlast = Bool()
  }
  val sQueue = Module(new Queue(new sQueueBundle(c.BUS_WIDTH), msgNum*c.MSG_DURATION))

  sQueue.io.enq.valid := io.sAxisIf.valid
  sQueue.io.enq.bits.tdata := io.sAxisIf.bits.tdata
  sQueue.io.enq.bits.tlast := io.sAxisIf.bits.tlast

  
  val correctMsg = startMsg & queueErrPos.io.deq.bits.ffs.orR

  // dscQueue is used to store message description. Is it corrupted ot not 

  val cntr = RegInit(UInt((log2Ceil(c.BUS_WIDTH * c.MSG_DURATION)+1).W), 0.U)
  val mReady = RegInit(Bool(),false.B)
  sQueue.io.deq.ready := mReady

  // Counter controlls READ operation from sQueue
  when(startMsg) {
    cntr := cntr + c.BUS_WIDTH.U
    mReady := true.B
  }.elsewhen(mReady){
    when(sQueue.io.deq.bits.tlast) {
      cntr := 0.U
      mReady := false.B
    }.otherwise{
      cntr := cntr + c.BUS_WIDTH.U
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
  val shiftEnableVec = Wire(Vec(c.BUS_WIDTH, UInt(1.W)))
  val shiftVal = shiftEnableVec.reduce(_+&_)
  
  val errPosVecRev = VecInit(queueErrPos.io.deq.bits.vec.reverse)
  val errValVecRev = VecInit(queueErrVal.io.deq.bits.reverse)

  val errPosAxis = Wire(Vec(c.BUS_WIDTH, UInt(log2Ceil(c.BUS_WIDTH).W)))
  val errPosVec = Reg(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
  val errValVec = Reg(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
  val errPosSel = Reg(UInt(c.T_LEN.W))

  val ffsCountOnes = c.SYMB_WIDTH.U - PopCount(queueErrPos.io.deq.bits.ffs)
  
  when(correctMsg) {
    errPosVec := (errPosVecRev.asUInt >> (c.SYMB_WIDTH.U * ffsCountOnes)).asTypeOf(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
    errValVec := (errValVecRev.asUInt >> (c.SYMB_WIDTH.U * ffsCountOnes)).asTypeOf(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
    errPosSel := Reverse(rsDecoder.io.errPosIf.bits.ffs) >> ffsCountOnes    
  }.elsewhen(shiftEnableVec.reduce(_|_) === 1.U){
    errPosVec := (errPosVec.asUInt >> (c.SYMB_WIDTH.U * shiftVal)).asTypeOf(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
    errValVec := (errValVec.asUInt >> (c.SYMB_WIDTH.U * shiftVal)).asTypeOf(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
    errPosSel := errPosSel >> shiftVal
  }

  // Shift enable
  for(i <- 0 until c.BUS_WIDTH) {
    errPosAxis(i) := errPosVec(i) % c.BUS_WIDTH.U
    when(errPosVec(i) < cntr) {
      shiftEnableVec(i) := errPosSel(i)
    }.otherwise{
      shiftEnableVec(i) := 0.U
    }
  }

  val mTdata = Wire(Vec(c.BUS_WIDTH, UInt(c.SYMB_WIDTH.W)))
  val mTkeep = Wire(UInt(c.BUS_WIDTH.W))

  mTdata := sQueue.io.deq.bits.tdata  

  when(sQueue.io.deq.bits.tlast){
    mTkeep := Fill(c.N_LEN % c.BUS_WIDTH, 1.U)
  }.otherwise{
    mTkeep := Fill(c.BUS_WIDTH, 1.U)
  }

  for(i <- 0 until c.BUS_WIDTH) {
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

