package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class BarrelShifter[T <: Data](shiftUnit: T, width: Int) extends Module {
  val io = IO(new Bundle {
    val vecIn   = Input(Vec(width, shiftUnit))
    val shiftVal = Input(UInt(log2Ceil(width + 1).W))
    val vecOut  = Output(Vec(width, shiftUnit))
  })

  // Define zero for shiftUnit
  val zeroShiftUnit = 0.U.asTypeOf(shiftUnit)

  // Create shifted versions of vecIn
  val shiftedVectors = VecInit(Seq.tabulate(width + 1) { shiftAmount =>
    if (shiftAmount == 0) {
      io.vecIn
    } else {
      val zeros = Seq.fill(shiftAmount)(zeroShiftUnit)
      val shiftedData = zeros ++ io.vecIn.take(width - shiftAmount)
      VecInit(shiftedData)
    }
  })

  // Select the shifted vector based on shiftVal
  val selectedShiftedVec = shiftedVectors(io.shiftVal)

  // Assign to vecOut
  io.vecOut := selectedShiftedVec
  
}


object GenBarrelShifter extends App {
  ChiselStage.emitSystemVerilogFile(new BarrelShifter(UInt(8.W), 8), Array())
}

