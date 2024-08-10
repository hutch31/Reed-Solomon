package Rs

import chisel3._
import chisel3.util._

class ShiftBundleMod[T <: Data](shiftUnit: T, width: Int, shiftVal: Int) extends Module{
  require(shiftVal <= width, "[ERROR] ShiftBundle. Wrong shiftVal compared to the width.")
  val io = IO(new Bundle {
    val vecIn = Input(Valid(Vec(width, shiftUnit)))
    val vecOut = Output(Valid(Vec(shiftVal, shiftUnit)))
    val lastOut = Output(Bool())
  })

  if(shiftVal == width) {
    io.vecOut.bits := io.vecIn.bits
    io.lastOut := io.vecIn.valid
    io.vecOut.valid := io.vecIn.valid
  } else {

    val vldWidth = math.ceil(width/shiftVal.toDouble).toInt
    val dataShift = Reg(Vec(width, shiftUnit))
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
      io.lastOut := 1.U
    }.otherwise {
      io.lastOut := 0.U
    }    

    for(i <- 0 until shiftVal){
      io.vecOut.bits(i) := dataShift(i)
    }
  }

}
