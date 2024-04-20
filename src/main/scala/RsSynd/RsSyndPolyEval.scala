package Rs

import chisel3._
import chisel3.util._

class RsSyndPolyEval extends Module with GfParams {
  val io = IO(new Bundle {
    val root = Input(UInt(symbWidth.W))
    val sAxisIf = Input(Valid(new axisIf(axisWidth)))
    val synd = Output(Valid(UInt(symbWidth.W)))
  })

  val cntrWidth = log2Ceil(nLen)
  val stageOut = Wire(Vec(axisWidth, UInt(symbWidth.W)))
  val termsSumVec = Wire(Vec(axisWidth, UInt(symbWidth.W)))
  val termSum = Wire(UInt(symbWidth.W))

  for(i <- 0 until axisWidth) {
    //power of X generation
    val powerDwnCntr = RegInit(UInt(cntrWidth.W), (nLen-i-1).U)
    when(io.sAxisIf.valid) {
      when(io.sAxisIf.bits.tlast) {
        powerDwnCntr := (nLen-i-1).U
      }.otherwise{
        powerDwnCntr := powerDwnCntr - axisWidth.U
      }
    }
    val xPower = gfPow(io.root, powerDwnCntr)
    stageOut(i) := gfMult(io.sAxisIf.bits.tdata(i), xPower)
  }

  termsSumVec := (stageOut zip io.sAxisIf.bits.tkeep.asTypeOf(Vec(axisWidth, Bool()))).map{case(a,b) => a & Fill(symbWidth,b) }
  termSum := termsSumVec.reduce(_^_)

  val accumQ = RegInit(UInt(symbWidth.W), 0.U)

  when(io.sAxisIf.valid) {
    when(io.sAxisIf.bits.tlast){
      accumQ := 0.U
    }.otherwise{
      accumQ := accumQ ^ termSum
    }    
  }

  val synd = Reg(UInt(symbWidth.W))
  val syndVld = RegInit(Bool(), false.B)

  when(io.sAxisIf.valid && io.sAxisIf.bits.tlast) {
    synd := accumQ ^ termSum
    syndVld := true.B
  }.otherwise{
    syndVld := false.B
  }

  io.synd.bits := synd
  io.synd.valid := syndVld

}
