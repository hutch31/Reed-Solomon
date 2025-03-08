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

class AlignInVec[T <: Data](shiftUnit: T, inWidth: Int, outWidth: Int) extends Module {

  val io = IO(new Bundle {
    val vecIn = Input(Vec(inWidth, shiftUnit))
    val shiftVal = Input(UInt(log2Ceil(inWidth+1).W))
    val vecOut  = Output(Vec(outWidth, shiftUnit))
  })

  // Assuming shiftUnit is already defined and inWidth, outWidth are given

  // Define zero for shiftUnit
  val zeroShiftUnit = 0.U.asTypeOf(shiftUnit)

  // Create vecInShift(inWidth = 4, outWidth = 8):
  // vecInShift[0] = [0, 0, 0, 0, 0, 0, 0, 0]
  // vecInShift[1] = [vecIn[0], 0, 0, 0, 0, 0, 0, 0]
  // vecInShift[2] = [vecIn[1], vecIn[0], 0, 0, 0, 0, 0, 0]
  // vecInShift[3] = [vecIn[2], vecIn[1], vecIn[0], 0, 0, 0, 0, 0]
  // vecInShift[4] = [vecIn[3], vecIn[2], vecIn[1], vecIn[0], 0, 0, 0, 0]

  // Create vecInShift
  val vecInShift = VecInit(Seq.tabulate(inWidth + 1) { i =>
    // For i == 0, return zero padding
    if (i == 0) {
      VecInit(Seq.fill(outWidth)(zeroShiftUnit))
    } else {
      // Create a sequence for each vecInShift[i]
      val elems = (0 until outWidth).map { j =>
        if (j < i) io.vecIn(i - 1 - j) else zeroShiftUnit
      }
      VecInit(elems)
    }
  })

  io.vecOut := vecInShift(io.shiftVal)

}

object GenAlignInVec extends App {
  ChiselStage.emitSystemVerilogFile(new AlignInVec(UInt(8.W), 4, 8), Array())
}
