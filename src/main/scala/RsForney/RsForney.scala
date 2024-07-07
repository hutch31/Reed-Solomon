package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

class RsForney extends Module with GfParams {
  val io = IO(new Bundle {
    val errPosIf = Input(Valid(new vecFfsIf(tLen)))
    val syndIf = Input(Valid(Vec(redundancy, UInt(symbWidth.W))))
    val errValIf = Output(Valid(new vecFfsIf(tLen)))
    val errPosOutIf = Output(Valid(new vecFfsIf(tLen)))
  })

  //////////////////////////////////////
  // need to convert the positions to coefficients
  // degrees for the errata locator algo to work
  // (eg: instead of [0, 1, 2] it will become [len(msg)-1, len(msg)-2, len(msg) -3])
  //////////////////////////////////////

  val ffs = Module(new FindFirstSet(tLen))
  ffs.io.in := io.errPosIf.bits.ffs
  val errPosCoefIf = Wire(Valid(new vecFfsIf(tLen)))
  errPosCoefIf.bits.ffs := ffs.io.out

  errPosCoefIf.valid := io.errPosIf.valid

  for(i <- 0 until tLen)
    errPosCoefIf.bits.vec(i) := (nLen-1).U-io.errPosIf.bits.vec(i)

  //////////////////////////////////////
  // Xl and XlInv
  //////////////////////////////////////

  val Xl = Wire(Vec(tLen, (UInt(symbWidth.W))))
  val XlInv = Wire(Vec(tLen, (UInt(symbWidth.W))))

  for(i <- 0 until tLen) {    
    val coefDiff = (symbNum-1).U-errPosCoefIf.bits.vec(i)
    Xl(i) := powerFirstRootNeg(coefDiff)
    XlInv(i) := gfInv(Xl(i))
  }

  val XlInvFfsIf = Wire(Valid(new vecFfsIf(tLen)))
  //val XlInvVldIf = Wire(Valid(new vecFfsIf(tLen)))

  XlInvFfsIf.bits.vec := XlInv
  XlInvFfsIf.bits.ffs := ffs.io.out
  XlInvFfsIf.valid := io.errPosIf.valid

  //////////////////////////////////////
  // Modules instantiation
  //////////////////////////////////////

  val errataLoc = Module(new ErrataLoc)
  val errEval = Module(new ErrEval)
  val errEvalXlInv = Module(new ErrEvalXlInv)
  val formalDer = Module(new FormalDerivative)
  val errVal = Module(new ErrVal)
  // TODO: Is 4 Entries enought ?
  // TODO: add queue
  val queueErrPos = Module(new Queue(new vecFfsIf(tLen), 4))
  // ErrataLocator
  errataLoc.io.errPosCoefIf <> errPosCoefIf
  errEval.io.errataLocIf <> errataLoc.io.errataLocIf
  errEval.io.syndIf := io.syndIf

  errEvalXlInv.io.errEvalIf <> errEval.io.errEvalIf
  errEvalXlInv.io.XlInvIf <> XlInvFfsIf

  formalDer.io.XlInvIf <> XlInvFfsIf
  formalDer.io.Xl := Xl

  errVal.io.formalDerIf <> formalDer.io.formalDerIf
  errVal.io.errEvalXlInvIf <> errEvalXlInv.io.errEvalXlInvIf
  errVal.io.Xl := Xl

  io.errValIf <> errVal.io.errValIf

  queueErrPos.io.enq.valid := io.errPosIf.valid
  queueErrPos.io.enq.bits.vec := io.errPosIf.bits.vec
  queueErrPos.io.enq.bits.ffs := io.errPosIf.bits.ffs

  queueErrPos.io.deq.ready := io.errValIf.valid
  io.errPosOutIf.bits.vec := queueErrPos.io.deq.bits.vec
  io.errPosOutIf.bits.ffs := queueErrPos.io.deq.bits.ffs
  io.errPosOutIf.valid := queueErrPos.io.deq.valid

}

// runMain Rs.GenForney

object GenForney extends App {
  //ChiselStage.emitSystemVerilogFile(new ErrataLocatorPar(), Array())
  //ChiselStage.emitSystemVerilogFile(new ErrEval(), Array())
  //ChiselStage.emitSystemVerilogFile(new ErrEvalXlInv(), Array())
  //ChiselStage.emitSystemVerilogFile(new ShiftTest(), Array())
  ChiselStage.emitSystemVerilogFile(new RsForney(), Array())
  //ChiselStage.emitSystemVerilogFile(new GfDiv(), Array())
  //ChiselStage.emitSystemVerilogFile(new FormalDerivative(), Array())
  //ChiselStage.emitSystemVerilogFile(new FormalDerivativeStage1(), Array())
}
