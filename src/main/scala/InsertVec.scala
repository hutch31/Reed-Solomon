/*
 * ----------------------------------------------------------------------
 *  Copyright (c) 2024 Egor Smirnov
 *
 *  Licensed under terms of the MIT license
 *  See https://github.com/egorman44/Reed-Solomon/blob/main/LICENSE
 *    for license terms
 * ----------------------------------------------------------------------
 */

package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class InsertVec[T <: Data](shiftUnit: T, inWidth: Int, outWidth: Int) extends Module {
  val io = IO(new Bundle {
    val baseVec  = Input(Vec(outWidth, shiftUnit))
    val vecIn = Input(Vec(inWidth, shiftUnit))
    val shiftVal = Input(UInt(log2Ceil(inWidth+1).W))
    val vecOut  = Output(Vec(outWidth, shiftUnit))
  })

  //val alignInVec = Module(new AlignInVec(shiftUnit, inWidth, outWidth))
  val barrelShifter = Module(new BarrelShifter(shiftUnit, outWidth))

  //alignInVec.io.vecIn := io.vecIn
  //alignInVec.io.shiftVal := io.shiftVal

  barrelShifter.io.vecIn := io.vecIn
  barrelShifter.io.shiftVal := io.shiftVal

  // Perform the OR operation element-wise
  for (i <- 0 until outWidth) {
    //io.vecOut(i) := (barrelShifter.io.vecOut(i).asUInt | alignInVec.io.vecOut(i).asUInt).asTypeOf(shiftUnit)
    io.vecOut(i) := (barrelShifter.io.vecOut(i).asUInt | io.baseVec(i).asUInt).asTypeOf(shiftUnit)
  }

}

object GenInsertVec extends App {
  class ShiftUnit extends Bundle {
    val formDerSymb = UInt(10.W)
    val errEvalXlInvSymb = UInt(8.W)
    val XlSymb = UInt(1.W)
  }
  ChiselStage.emitSystemVerilogFile(new InsertVec(new ShiftUnit, 4, 8), Array())
}
