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
    val errorEval = Output(Valid(Vec(tLen, UInt(symbWidth.W))))
  })

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
  // Modules instantiation
  //////////////////////////////////////

  val errataLocator = Module(new ErrataLocatorPar)
  val errorEvaluator = Module(new ErrorEvaluator)
  val formalDer = Module(new FormalDerivative)

  // ErrataLocator
  errataLocator.io.errPosCoefIf <> errPosCoefIf
  errorEvaluator.io.errataLocIf <> errataLocator.io.errataLocIf
  errorEvaluator.io.syndrome := io.syndrome

  formalDer.io.errPosCoefIf <> errPosCoefIf

  io.formalDerIf := formalDer.io.formalDerIf
  io.errorEval := errorEvaluator.io.errorEval

}

object GenForney extends App {
  //ChiselStage.emitSystemVerilogFile(new ErrataLocatorPar(), Array())
  //ChiselStage.emitSystemVerilogFile(new ErrorEvaluator(), Array())
  ChiselStage.emitSystemVerilogFile(new RsForney(), Array())
  //ChiselStage.emitSystemVerilogFile(new FormalDerivative(), Array())
  //ChiselStage.emitSystemVerilogFile(new FormalDerivativeStage1(), Array())
}
