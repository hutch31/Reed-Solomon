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

class RsBmWrap(c: Config) extends Module {
  val io = IO(new Bundle {
    val syndIf = Input(Valid(Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W))))
    val errLocIf = Output(Valid(new vecFfsIf(c.T_LEN+1, c.SYMB_WIDTH)))
  })

  // RsBm has a static latency, so it's safe to use it
  // in a ping-pong mode to speed up BM algorithm.

  val rsBm = for (i <- 0 until c.rsBmNum) yield Module (new RsBm(c))

  val bmValid = Wire(UInt(c.rsBmNum.W))
  val bmSel = Wire(UInt(c.rsBmNum.W))
  val bmSelQ = RegInit(UInt(c.rsBmNum.W), 1.U)

  when(io.syndIf.valid) {
    bmSel := (bmSelQ << 1) | (bmSelQ >> (c.rsBmNum-1))
    bmSelQ := bmSel
  }.otherwise{
    bmSel := bmSelQ
  }

  if(c.rsBmNum == 1) bmValid := io.syndIf.valid
  else bmValid := bmSel & Fill(c.rsBmNum, io.syndIf.valid)


  for(i <- 0 until c.rsBmNum) {
    rsBm(i).io.syndIf.bits := io.syndIf.bits
    rsBm(i).io.syndIf.valid := bmValid(i)
  }

  val errLocSel = Wire(UInt(c.rsBmNum.W))
  val errLocSelQ = RegInit(UInt(c.rsBmNum.W), 1.U)
  val errLocValid = (VecInit(rsBm.map(_.io.errLocIf.valid))).asTypeOf(UInt(c.rsBmNum.W))

  when(io.errLocIf.valid) {
    errLocSel := errLocValid
    errLocSelQ := errLocValid
  }.otherwise {
    errLocSel := errLocSelQ
  }

  io.errLocIf.valid := errLocValid.orR
  io.errLocIf.bits := Mux1H(errLocSel, rsBm.map(_.io.errLocIf.bits))

}
