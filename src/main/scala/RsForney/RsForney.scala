package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

class RsForney(c: Config) extends Module {
  val io = IO(new Bundle {
    val errPosIf = Input(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val syndIf = Input(Valid(Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W))))
    val errValIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
  })

  //////////////////////////////////////
  // need to convert the positions to coefficients
  // degrees for the errata locator algo to work
  // (eg: instead of [0, 1, 2] it will become [len(msg)-1, len(msg)-2, len(msg) -3])
  //////////////////////////////////////

  val ffs = Module(new FindFirstSet(c.T_LEN))
  ffs.io.in := io.errPosIf.bits.ffs
  val errPosCoefIf = Wire(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
  errPosCoefIf.bits.ffs := ffs.io.out

  errPosCoefIf.valid := io.errPosIf.valid

  for(i <- 0 until c.T_LEN)
    errPosCoefIf.bits.vec(i) := (c.N_LEN-1).U-io.errPosIf.bits.vec(i)

  //////////////////////////////////////
  // Xl and XlInv
  //////////////////////////////////////

  val Xl = Wire(Vec(c.T_LEN, (UInt(c.SYMB_WIDTH.W))))
  val XlInv = Wire(Vec(c.T_LEN, (UInt(c.SYMB_WIDTH.W))))

  for(i <- 0 until c.T_LEN) {    
    val coefDiff = (c.SYMB_NUM-1).U-errPosCoefIf.bits.vec(i)
    Xl(i) := c.powerFirstRootNeg(coefDiff)
    XlInv(i) := c.gfInv(Xl(i))
  }

  val XlInvFfsIf = Wire(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
  //val XlInvVldIf = Wire(Valid(new vecFfsIf(c.T_LEN)))

  XlInvFfsIf.bits.vec := XlInv
  XlInvFfsIf.bits.ffs := ffs.io.out
  XlInvFfsIf.valid := io.errPosIf.valid

  //////////////////////////////////////
  // Modules instantiation
  //////////////////////////////////////

  val errataLoc = Module(new ErrataLoc(c))
  val errEval = Module(new ErrEval(c))
  val errEvalXlInv = Module(new ErrEvalXlInv(c))
  val formalDer = Module(new FormalDerivative(c))
  val errVal = Module(new ErrVal(c))

  errataLoc.io.errPosCoefIf <> errPosCoefIf
  errEval.io.errataLocIf <> errataLoc.io.errataLocIf

  //////////////////////////////////////
  // Insert FIFO if required
  //////////////////////////////////////

  /////////////////
  // Syndrome FIFO for ErrEval
  /////////////////
  if(c.forneySyndFifoEn) {
    val queueSynd = Module(new Queue(Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W)), c.forneySyndFifoDepth))
    queueSynd.io.enq.valid := io.syndIf.valid
    queueSynd.io.enq.bits := io.syndIf.bits
    queueSynd.io.deq.ready := errEval.io.errEvalShiftCompleted
    errEval.io.syndIf.bits := queueSynd.io.deq.bits
    val queueSyndFullWrite = !queueSynd.io.enq.ready && queueSynd.io.enq.valid && !reset.asBool
    assert(!queueSyndFullWrite, "[ERROR] Write error into full queueSynd FIFO. Encrease FIFO depth.")
  } else {
    errEval.io.syndIf.bits := io.syndIf.bits
  }
  errEval.io.syndIf.valid := 0.U // Don't need valid of synd

  /////////////////
  // XlInvIf FIFO to EEXlInv
  /////////////////

  if(c.XlInvIfToEEXlInvFifoEn) {
    val queueXlInvIfToEEXlInv = Module(new Queue(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH), c.XlInvIfToEEXlInvFifoDepth))
    queueXlInvIfToEEXlInv.io.enq.valid := XlInvFfsIf.valid
    queueXlInvIfToEEXlInv.io.enq.bits.vec := XlInvFfsIf.bits.vec
    queueXlInvIfToEEXlInv.io.enq.bits.ffs := XlInvFfsIf.bits.ffs
    queueXlInvIfToEEXlInv.io.deq.ready := errEval.io.errEvalIf.valid
    errEvalXlInv.io.XlInvIf.bits.vec := queueXlInvIfToEEXlInv.io.deq.bits.vec
    errEvalXlInv.io.XlInvIf.bits.ffs := queueXlInvIfToEEXlInv.io.deq.bits.ffs
    errEvalXlInv.io.XlInvIf.valid    := queueXlInvIfToEEXlInv.io.deq.valid
    val queueXlInvIfToEEXlInvFullWrite = !queueXlInvIfToEEXlInv.io.enq.ready && queueXlInvIfToEEXlInv.io.enq.valid && !reset.asBool
    assert(!queueXlInvIfToEEXlInvFullWrite, "[ERROR] Write error into full queueXlInvIfToEEXlInv FIFO. Encrease FIFO depth.")
  } else {
    errEvalXlInv.io.XlInvIf <> XlInvFfsIf
  }
  errEvalXlInv.io.errEvalIf <> errEval.io.errEvalIf

  /////////////////
  // formalDer FIFO to errVal
  /////////////////

  formalDer.io.XlInvIf <> XlInvFfsIf
  formalDer.io.Xl := Xl


  /////////////////
  // Xl FIFO to errVal
  /////////////////

  if(c.XlIfToEvFifoEn) {
    val queueXlIfToEv = Module(new Queue(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)), c.XlIfToEvFifoDepth))
    queueXlIfToEv.io.enq.valid := XlInvFfsIf.valid
    queueXlIfToEv.io.enq.bits  := Xl
    // Xl is connected to the shift reg which captures on the errEvalXlInvIf.valid signal
    queueXlIfToEv.io.deq.ready := errVal.io.errValIf.valid
    errVal.io.Xl               := queueXlIfToEv.io.deq.bits
    val queueXlIfToEvFullWrite = !queueXlIfToEv.io.enq.ready && queueXlIfToEv.io.enq.valid && !reset.asBool
    assert(!queueXlIfToEvFullWrite, "[ERROR] Write error into full queueXlIfToEv FIFO. Encrease FIFO depth.")
  } else {
    errVal.io.Xl := Xl
  }

  /////////////////
  // FD FIFO to errVal
  /////////////////

  if(c.FdToEvFifoEn) {
    val queueFdToEv = Module(new Queue(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)), c.XlIfToEvFifoDepth))
    queueFdToEv.io.enq.valid := formalDer.io.formalDerIf.valid
    queueFdToEv.io.enq.bits  := formalDer.io.formalDerIf.bits
    // Xl is connected to the shift reg which captures on the errEvalXlInvIf.valid signal
    queueFdToEv.io.deq.ready := errVal.io.errValShiftCompleted
    errVal.io.formalDerIf    := queueFdToEv.io.deq.bits
    val queueFdToEvFullWrite = !queueFdToEv.io.enq.ready && queueFdToEv.io.enq.valid && !reset.asBool
    assert(!queueFdToEvFullWrite, "[ERROR] Write error into full queueFdToEv FIFO. Encrease FIFO depth.")
  } else {
    errVal.io.formalDerIf <> formalDer.io.formalDerIf.bits
  }

  errVal.io.errEvalXlInvIf <> errEvalXlInv.io.errEvalXlInvIf

  io.errValIf <> errVal.io.errValIf

}

// runMain Rs.GenForney

object GenForney extends App {
  val c = JsonReader.readConfig("/home/egorman44/chisel-lib/rs.json")
  ChiselStage.emitSystemVerilogFile(new RsForney(c), Array())  
}
