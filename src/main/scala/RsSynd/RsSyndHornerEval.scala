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

class RsSyndHornerEval(c: Config) extends Module {
  val io = IO(new Bundle {
    val root = Input(UInt(c.SYMB_WIDTH.W))
    val sAxisIf = Input(Valid(new axisIf(c.BUS_WIDTH, c.SYMB_WIDTH)))
    val synd = Output(UInt(c.SYMB_WIDTH.W))
  })

  val sop = io.sAxisIf.valid && RegNext(next=io.sAxisIf.valid, false.B)
  // Horner method check https://en.wikiversity.org/wiki/Reed%E2%80%93Solomon_codes_for_coders

  val gfMultIntrm = Wire(Vec(c.BUS_WIDTH, UInt(c.SYMB_WIDTH.W)))
  val xorIntrm = Wire(Vec(c.BUS_WIDTH, UInt(c.SYMB_WIDTH.W)))
  val accumQ = RegInit(UInt(c.SYMB_WIDTH.W), 0.U)

  for(i <- 0 until c.BUS_WIDTH) {
    if(i == 0) {
      when(sop) {
        gfMultIntrm(0) := 0.U
        xorIntrm(0) := io.sAxisIf.bits.tdata(0)
      }.otherwise {
        gfMultIntrm(i) := c.gfMult(accumQ, io.root)
        xorIntrm(i) := gfMultIntrm(i) ^ io.sAxisIf.bits.tdata(i)
      }
    } else {
      gfMultIntrm(i) := c.gfMult(xorIntrm(i-1), io.root)
      xorIntrm(i) := gfMultIntrm(i) ^ io.sAxisIf.bits.tdata(i)
    }
  }

  val ffs = Module(new FindFirstSet(c.BUS_WIDTH))
  ffs.io.in := io.sAxisIf.bits.tkeep
  val cycleSum = Mux1H(ffs.io.out, xorIntrm)

  when(io.sAxisIf.valid && io.sAxisIf.bits.tlast) {
    accumQ := 0.U
  }.otherwise{
    accumQ := cycleSum
  }

  io.synd := cycleSum

}
