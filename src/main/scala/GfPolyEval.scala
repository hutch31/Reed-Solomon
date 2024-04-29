package Rs

import chisel3._
import chisel3.util._

class GfPolyEvalHorner(vecLen : Int, comboLen : Int) extends Module with GfParams {

  val qEn = if(comboLen < vecLen) true else false

  val io = IO(new Bundle {
    val coefVec = Input(Valid(new DataSelBundle(vecLen)))
    val x = Input(UInt(symbWidth.W))
    val evalValue = Output(Valid(UInt(symbWidth.W)))
  })

  class MuxInBundle extends Bundle {
    val data = UInt(symbWidth.W)
    val valid = UInt(1.W)
  }

  val combStage = for(i <- 0 until vecLen-1) yield Module(new GfPolyEvalHornerStage)
  val combVld = Wire(Vec(vecLen-1, Bool()))
  val combX = Wire(Vec(vecLen-1, UInt(symbWidth.W)))

  val muxIn = Wire(Vec(vecLen-1, new MuxInBundle))
  dontTouch(muxIn)

  combStage(0).io.x := io.x
  combStage(0).io.coef := io.coefVec.bits.data(1)
  combStage(0).io.prev := io.coefVec.bits.data(0)
  combVld(0) := io.coefVec.valid
  combX(0) := io.x

  muxIn(0).data := combStage(0).io.next
  muxIn(0).valid := combVld(0)

  for(i <- 1 until vecLen-1) {
    combStage(i).io.coef := io.coefVec.bits.data(i+1)
    if(i%comboLen == 0) {
      val qStage = Reg(UInt(symbWidth.W))
      val qX = Reg(UInt(symbWidth.W))      
      val qVld = Reg(Bool())
      qStage := combStage(i-1).io.next
      combStage(i).io.prev := qStage
      qVld := combVld(i-1)
      combVld(i) := qVld
      qX := combX(i-1)
      combStage(i).io.x := qX
      combX(i) := qX
    } else {
      combStage(i).io.prev := combStage(i-1).io.next
      combStage(i).io.x := combX(i-1)
      combVld(i) := combVld(i-1)
      combX(i) := combX(i-1)      
    }
    muxIn(i).data := combStage(i).io.next
    muxIn(i).valid := combVld(i)
  }

  val muxOut = Mux1H(io.coefVec.bits.sel(vecLen-1,1), muxIn)

  io.evalValue.bits := muxOut.data
  io.evalValue.valid := muxOut.valid

}

class GfPolyEvalHornerStage extends Module with GfParams {
  val io = IO(new Bundle {
    val x = Input(UInt(symbWidth.W))
    val coef = Input(UInt(symbWidth.W))
    val prev = Input(UInt(symbWidth.W))
    val next = Output(UInt(symbWidth.W))
  })

  val mult = gfMult(io.x, io.prev)
  io.next := mult ^ io.coef

}

