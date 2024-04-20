package Rs

import chisel3._
import chisel3.util._

class RsSyndHornerEval extends Module with GfParams {
  val io = IO(new Bundle {
    val root = Input(UInt(symbWidth.W))
    val sAxisIf = Input(Valid(new axisIf(axisWidth)))
    val synd = Output(UInt(symbWidth.W))
  })

  val sop = io.sAxisIf.valid && RegNext(next=io.sAxisIf.valid, false.B)
  // Horner method check https://en.wikiversity.org/wiki/Reed%E2%80%93Solomon_codes_for_coders

  val gfMultIntrm = Wire(Vec(axisWidth, UInt(symbWidth.W)))
  val xorIntrm = Wire(Vec(axisWidth, UInt(symbWidth.W)))
  val accumQ = RegInit(UInt(symbWidth.W), 0.U)

  for(i <- 0 until axisWidth) {
    if(i == 0) {
      when(sop) {
        gfMultIntrm(0) := 0.U
        xorIntrm(0) := io.sAxisIf.bits.tdata(0)
      }.otherwise {
        gfMultIntrm(i) := gfMult(accumQ, io.root)
        xorIntrm(i) := gfMultIntrm(i) ^ io.sAxisIf.bits.tdata(i)
      }
    } else {
      gfMultIntrm(i) := gfMult(xorIntrm(i-1), io.root)
      xorIntrm(i) := gfMultIntrm(i) ^ io.sAxisIf.bits.tdata(i)
    }
  }

  val ffs = Module(new FindFirstSet(axisWidth))
  ffs.io.in := io.sAxisIf.bits.tkeep
  val cycleSum = Mux1H(ffs.io.out, xorIntrm)

  when(io.sAxisIf.valid && io.sAxisIf.bits.tlast) {
    accumQ := 0.U
  }.otherwise{
    accumQ := cycleSum
  }

  io.synd := cycleSum

}
