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
  // TODO: Is 4 Entries enought ?
  // TODO: add queue
  //val queueErrPos = Module(new Queue(new vecFfsIf(c.T_LEN), 4))
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

}

// runMain Rs.GenForney

object GenForney extends App {
  val c = JsonReader.readConfig("/home/egorman44/chisel-lib/rs.json")
  //ChiselStage.emitSystemVerilogFile(new ErrataLocatorPar(), Array())
  //ChiselStage.emitSystemVerilogFile(new ErrEval(), Array())
  //ChiselStage.emitSystemVerilogFile(new ErrEvalXlInv(), Array())
  //ChiselStage.emitSystemVerilogFile(new ShiftTest(), Array())
  ChiselStage.emitSystemVerilogFile(new RsForney(c), Array())
  //ChiselStage.emitSystemVerilogFile(new GfDiv(), Array())
  //ChiselStage.emitSystemVerilogFile(new FormalDerivative(), Array())
  //ChiselStage.emitSystemVerilogFile(new FormalDerivativeStage1(), Array())
}
