package Rs

import chisel3._
import chisel3.util._

class ErrVal extends Module with GfParams {
  val io = IO(new Bundle {
    val formalDerIf    = Input(Vec(tLen, UInt(symbWidth.W)))
    val errEvalXlInvIf = Input(Valid(new vecFfsIf(tLen)))
    val Xl = Input(Vec(tLen, (UInt(symbWidth.W))))
  })

  for(i <- 0 until tLen) {
    
  }
}
