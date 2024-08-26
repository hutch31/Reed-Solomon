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

  val xPower = Wire(Vec(c.BUS_WIDTH, UInt(c.SYMB_WIDTH.W)))
  val sAxisTdata  = Wire(Vec(c.BUS_WIDTH, UInt(c.SYMB_WIDTH.W)))
  val sAxisTkeep = Wire(UInt(c.BUS_WIDTH.W))

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
    // Pipeline syndrome calculation
    if(c.syndPipeEn) {
      xPower(i) := RegNext(c.gfPow(io.root, powerDwnCntr), init=0.U)
      sAxisTdata(i) := RegNext(io.sAxisIf.bits.tdata(i), init=0.U)
    } else {
      xPower(i) := c.gfPow(io.root, powerDwnCntr)
      sAxisTdata(i) := io.sAxisIf.bits.tdata(i)
    }

    stageOut(i) := c.gfMult(sAxisTdata(i), xPower(i))
  }

  val accumQ = RegInit(UInt(c.SYMB_WIDTH.W), 0.U)

  //
  val accumVld = Wire(Bool())
  val accumLast = Wire(Bool())

  if(c.syndPipeEn) {
    accumVld := RegNext(io.sAxisIf.valid, init=false.B)
    accumLast := RegNext(io.sAxisIf.bits.tlast, init=false.B)
    sAxisTkeep := RegNext(io.sAxisIf.bits.tkeep, init=0.U)
  } else {
    accumVld := io.sAxisIf.valid
    accumLast := io.sAxisIf.bits.tlast
    sAxisTkeep := io.sAxisIf.bits.tkeep
  }

  termsSumVec := (stageOut zip sAxisTkeep.asTypeOf(Vec(c.BUS_WIDTH, Bool()))).map{case(a,b) => a & Fill(c.SYMB_WIDTH,b) }
  termSum := termsSumVec.reduce(_^_)

  when(accumVld) {
    when(accumLast){
      accumQ := 0.U
    }.otherwise{
      accumQ := accumQ ^ termSum
    }    
  }

  val synd = Reg(UInt(c.SYMB_WIDTH.W))
  val syndVld = RegInit(Bool(), false.B)

  // Capture syndrome value
  when(accumVld && accumLast) {
    synd := accumQ ^ termSum
    syndVld := true.B
  }.otherwise{
    syndVld := false.B
  }

  io.syndIf.bits := synd
  io.syndIf.valid := syndVld

}
