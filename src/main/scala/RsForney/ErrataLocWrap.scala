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

  val errataLocValid = Wire(UInt(c.errataLocNum.W))
  val errataLocSel = Wire(UInt(c.errataLocNum.W))
  val errataLocSelQ = RegInit(UInt(c.errataLocNum.W), 1.U)

  when(io.errPosCoefIf.valid) {
    errataLocSel := (errataLocSelQ << 1) | (errataLocSelQ >> (c.errataLocNum-1))
    errataLocSelQ := errataLocSel
  }.otherwise{
    errataLocSel := errataLocSelQ
  }

  if(c.errataLocNum == 1) errataLocValid := io.errPosCoefIf.valid
  else errataLocValid := errataLocSel & Fill(c.errataLocNum, io.errPosCoefIf.valid)


  for(i <- 0 until c.errataLocNum) {
    errataLoc(i).io.errPosCoefIf.bits := io.errPosCoefIf.bits
    errataLoc(i).io.errPosCoefIf.valid := errataLocValid(i)
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
