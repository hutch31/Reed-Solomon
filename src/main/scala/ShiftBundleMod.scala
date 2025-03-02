package Rs

import chisel3._
import chisel3.util._

class ShiftBundleMod[T <: Data](shiftUnit: T, width: Int, shiftVal: Int, shiftCntrLimit: Int=0) extends Module{
  require(shiftVal <= width, "[ERROR] ShiftBundle. Wrong shiftVal compared to the width.")
  val io = IO(new Bundle {
    val vecIn = Input(Valid(Vec(width, shiftUnit)))
    val vecOut = Output(Valid(Vec(shiftVal, shiftUnit)))
    val lastOut = Output(Bool())
  })

  val shiftCntrQ = RegInit(UInt(log2Ceil(width).W), 0.U)
  val shiftEn = Wire(UInt(1.W))

  when(io.vecIn.valid) {
    shiftCntrQ := 0.U
  }.otherwise{
    when(shiftCntrQ === shiftCntrLimit.U) {
      shiftCntrQ := 0.U
    }.otherwise {
      shiftCntrQ := shiftCntrQ + 1.U
    }
  }

  if(shiftCntrLimit == 0){
    shiftEn := 1.U
  }else{
    shiftEn := (shiftCntrQ === shiftCntrLimit.U)
  }

  if(shiftVal == width) {
    io.vecOut.bits := RegNext(next=io.vecIn.bits)
    io.lastOut := RegNext(next=io.vecIn.valid)
    io.vecOut.valid := RegNext(next=io.vecIn.valid, init=false.B)
  } else {

    val vldWidth  = math.ceil(width/shiftVal.toDouble).toInt
    val dataShift = Reg(Vec(width, shiftUnit))
    val vldShift  = RegInit(UInt(vldWidth.W), 0.U)

    // Load data into the shift register
    when(io.vecIn.valid) {
      dataShift := io.vecIn.bits
      vldShift := ((1<<vldWidth)-1).U
    }.elsewhen(shiftEn === 1.U) {
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
