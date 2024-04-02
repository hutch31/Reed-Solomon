package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

class RsForney extends Module with GfParams {
  val io = IO(new Bundle {
    val posArray = Input(new NumPosIf)
    val syndrome = Input(Vec(redundancy, UInt(symbWidth.W)))
    val formalDerIf    = Output(Vec(tLen, UInt(symbWidth.W)))
    val errEvalXlInvIf = Output(Valid(new vecFfsIf(tLen)))
  })

  //////////////////////////////////////
  //  INTERFACES
  //////////////////////////////////////

  //////////////////////////////////////
  // need to convert the positions to coefficients
  // degrees for the errata locator algo to work
  // (eg: instead of [0, 1, 2] it will become [len(msg)-1, len(msg)-2, len(msg) -3])
  //////////////////////////////////////

  val ffs = Module(new FindFirstSet(tLen))
  ffs.io.in := io.posArray.sel

  val errPosCoefIf = Wire(Valid(new vecFfsIf(tLen)))
  errPosCoefIf.bits.ffs := ffs.io.out
  errPosCoefIf.valid := io.posArray.valid

  for(i <- 0 until tLen)
    errPosCoefIf.bits.vec(i) := (nLen-1).U-io.posArray.pos(i)

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
  XlInvFfsIf.valid := io.posArray.valid

  //XlInvVldIf.bits.vec := XlInv
  //XlInvVldIf.valid := io.posArray.valid

  //////////////////////////////////////
  // Modules instantiation
  //////////////////////////////////////

  val errataLocator = Module(new ErrataLocatorPar)
  val errorEvaluator = Module(new ErrEval)
  val errEvalXlInv = Module(new ErrEvalXlInv)
  val formalDer = Module(new FormalDerivative)

  // ErrataLocator
  errataLocator.io.errPosCoefIf <> errPosCoefIf
  errorEvaluator.io.errataLocIf <> errataLocator.io.errataLocIf
  errorEvaluator.io.syndrome := io.syndrome

  errEvalXlInv.io.errEvalIf <> errorEvaluator.io.errEvalIf
  errEvalXlInv.io.XlInvIf <> XlInvFfsIf

  formalDer.io.XlInvIf <> XlInvFfsIf
  formalDer.io.Xl := Xl

  io.errEvalXlInvIf <> errEvalXlInv.io.errEvalXlInvIf
  io.formalDerIf := formalDer.io.formalDerIf

  
}

// runMain Rs.GenForney

object GenForney extends App {
  //ChiselStage.emitSystemVerilogFile(new ErrataLocatorPar(), Array())
  //ChiselStage.emitSystemVerilogFile(new ErrEval(), Array())
  //ChiselStage.emitSystemVerilogFile(new ErrEvalXlInv(), Array())
  //ChiselStage.emitSystemVerilogFile(new ShiftTest(), Array())
  ChiselStage.emitSystemVerilogFile(new RsForney(), Array())
  //ChiselStage.emitSystemVerilogFile(new FormalDerivative(), Array())
  //ChiselStage.emitSystemVerilogFile(new FormalDerivativeStage1(), Array())
}
