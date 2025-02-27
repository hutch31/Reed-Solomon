package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class ErrataLocWrap(c: Config) extends Module {
  val io = IO(new Bundle {
    val errPosCoefIf = Input(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val errataLocIf = Output(Valid(new vecFfsIf(c.T_LEN+1, c.SYMB_WIDTH)))
  })

  // errataLoc has a static latency, so it's safe to use it
  // in a ping-pong mode to speed up errataLoc algorithm.

  val errataLoc = for (i <- 0 until c.errataLocNum) yield Module (new ErrataLoc(c))

  val errPosSel = Wire(UInt(c.errataLocNum.W))
  val errPosSelQ = RegInit(UInt(c.errataLocNum.W), 1.U)
  
  when(io.errPosCoefIf.valid) {
    errPosSel := (errPosSelQ << 1) | (errPosSelQ >> (c.errataLocNum-1))
    errPosSelQ := errPosSel
  }.otherwise{
    errPosSel := errPosSelQ
  }

  val errPosCoefVld = Wire(UInt(c.errataLocNum.W))  
  if(c.errataLocNum == 1) errPosCoefVld := io.errPosCoefIf.valid
  else errPosCoefVld := errPosSel & Fill(c.errataLocNum, io.errPosCoefIf.valid)

  // Input ports connection
  for(i <- 0 until c.errataLocNum) {
    errataLoc(i).io.errPosCoefIf.bits := io.errPosCoefIf.bits
    errataLoc(i).io.errPosCoefIf.valid := errPosCoefVld(i)
  }

  val errLocSel = Wire(UInt(c.errataLocNum.W))
  val errLocSelQ = RegInit(UInt(c.errataLocNum.W), 1.U)
  val errLocValid = (VecInit(errataLoc.map(_.io.errataLocIf.valid))).asTypeOf(UInt(c.errataLocNum.W))

  when(io.errataLocIf.valid) {
    errLocSel := errLocValid
    errLocSelQ := errLocValid
  }.otherwise {
    errLocSel := errLocSelQ
  }

  io.errataLocIf.valid := errLocValid.orR
  io.errataLocIf.bits := Mux1H(errLocSel, errataLoc.map(_.io.errataLocIf.bits))

}
