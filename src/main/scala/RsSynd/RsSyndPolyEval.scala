package Rs

import chisel3._
import chisel3.util._

class RsSyndPolyEval(c: Config) extends Module{
  val io = IO(new Bundle {
    val root = Input(UInt(c.SYMB_WIDTH.W))
    val sAxisIf = Input(Valid(new axisIf(c.BUS_WIDTH, c.SYMB_WIDTH)))
    val syndIf = Output(Valid(UInt(c.SYMB_WIDTH.W)))
  })

  val cntrWidth = log2Ceil(c.N_LEN)
  val stageOut = Wire(Vec(c.BUS_WIDTH, UInt(c.SYMB_WIDTH.W)))
  val termsSumVec = Wire(Vec(c.BUS_WIDTH, UInt(c.SYMB_WIDTH.W)))
  val termSum = Wire(UInt(c.SYMB_WIDTH.W))

  for(i <- 0 until c.BUS_WIDTH) {
    //power of X generation
    val powerDwnCntr = RegInit(UInt(cntrWidth.W), (c.N_LEN-i-1).U)
    when(io.sAxisIf.valid) {
      when(io.sAxisIf.bits.tlast) {
        powerDwnCntr := (c.N_LEN-i-1).U
      }.otherwise{
        powerDwnCntr := powerDwnCntr - c.BUS_WIDTH.U
      }
    }
    val xPower = c.gfPow(io.root, powerDwnCntr)
    stageOut(i) := c.gfMult(io.sAxisIf.bits.tdata(i), xPower)
  }

  termsSumVec := (stageOut zip io.sAxisIf.bits.tkeep.asTypeOf(Vec(c.BUS_WIDTH, Bool()))).map{case(a,b) => a & Fill(c.SYMB_WIDTH,b) }
  termSum := termsSumVec.reduce(_^_)

  val accumQ = RegInit(UInt(c.SYMB_WIDTH.W), 0.U)

  when(io.sAxisIf.valid) {
    when(io.sAxisIf.bits.tlast){
      accumQ := 0.U
    }.otherwise{
      accumQ := accumQ ^ termSum
    }    
  }

  val synd = Reg(UInt(c.SYMB_WIDTH.W))
  val syndVld = RegInit(Bool(), false.B)

  when(io.sAxisIf.valid && io.sAxisIf.bits.tlast) {
    synd := accumQ ^ termSum
    syndVld := true.B
  }.otherwise{
    syndVld := false.B
  }

  io.syndIf.bits := synd
  io.syndIf.valid := syndVld

}
