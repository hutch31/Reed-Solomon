package Rs

import chisel3._
import chisel3.util._

class ShiftBundle(shiftVal: Int, symbWidth: Int) extends Bundle {
  val vec = Vec(shiftVal, UInt(symbWidth.W))
  val last = UInt(1.W)
}

class ShiftVec(width: Int, symbWidth: Int, shiftVal: Int) extends Module{
  require(shiftVal <= width, "[ERROR] ShiftVec. Wrong shiftVal compared to the width.")
  val io = IO(new Bundle {
    val vecIn = Input(Valid(Vec(width, UInt(symbWidth.W))))
    val vecOut = Output(Valid(new ShiftBundle(shiftVal, symbWidth)))
  })

  if(shiftVal == width) {
    io.vecOut.bits.vec := io.vecIn.bits
    io.vecOut.bits.last := io.vecIn.valid
    io.vecOut.valid := io.vecIn.valid
  } else {

    val vldWidth = math.ceil(width/shiftVal.toDouble).toInt
    val dataShift = Reg(Vec(width, (UInt(symbWidth.W))))
    val vldShift = RegInit(UInt(vldWidth.W), 0.U)

    // Load data into the shift register
    when(io.vecIn.valid) {
      dataShift := io.vecIn.bits
      vldShift := ((1<<vldWidth)-1).U
    }.otherwise {
      vldShift := vldShift >> 1
      for(i <- 0 until width-shiftVal)
        dataShift(i) := dataShift(i+shiftVal)
    }

    io.vecOut.valid := vldShift.asTypeOf(Vec(vldWidth, Bool())).reduce(_|_)

    when(vldShift === 1.U) {
      io.vecOut.bits.last := 1.U
    }.otherwise {
      io.vecOut.bits.last := 0.U
    }    

    for(i <- 0 until shiftVal){
      io.vecOut.bits.vec(i) := dataShift(i)
    }
  }


}

