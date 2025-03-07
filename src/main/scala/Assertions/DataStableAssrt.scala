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
import chisel3.util._

class DataStableAssrt[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val stop = Input(Bool())
    val data = Input(gen)
  })

  val checkEn = RegInit(false.B)
  val prevData = Reg(gen)

  when(io.start && !io.stop) {
    prevData := io.data
    checkEn := true.B
  }.elsewhen(io.stop){
    checkEn := false.B
  }

  val stableError = checkEn & !io.start & prevData.asUInt =/= io.data.asUInt
  dontTouch(stableError)
  assert(!stableError, "[ERROR] Data is not stable")

}

class NotReadyAssrt(Overlap: Boolean=false) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val stop = Input(Bool())
  })

  val busy = RegInit(Bool(), false.B)

  when(io.start && !io.stop) {
    busy := true.B
  }.elsewhen(io.stop) {
    busy := false.B
  }

  val notReady = if(Overlap) busy & io.start & !io.stop else busy & io.start
  dontTouch(notReady)
  assert(!notReady, "[ERROR] Logic is not ready to handle next data")
}
