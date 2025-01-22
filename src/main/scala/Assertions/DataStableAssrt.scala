package Rs

import chisel3._
import chisel3.util._

class DataStableAssrt[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val data = Input(gen)
  })

  val checkEn = RegInit(false.B)
  val prevData = Reg(gen)

  when(io.start) {
    prevData := io.data
    checkEn := true.B
  }

  val stableError = checkEn & ~io.start & prevData.asUInt =/= io.data.asUInt
  assert(!stableError, "[ERROR] Data is not stable")

}
