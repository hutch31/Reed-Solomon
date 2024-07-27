package Rs

import chisel3._
import chisel3.util._

// TODO: add GfPolyEvalBase parent class and two childs GfPolyEvalHorner/GfPolyEval.

class GfPolyEvalHorner(c: Config, vecLen : Int, comboLen : Int, selEn : Boolean=false) extends Module {

  val io = IO(new Bundle {
    val coefVec = Input(Valid(Vec(vecLen, UInt(c.SYMB_WIDTH.W))))
    val coefFfs = if(selEn) Some(Input(UInt(vecLen.W))) else None
    val x = Input(UInt(c.SYMB_WIDTH.W))
    val evalValue = Output(Valid(UInt(c.SYMB_WIDTH.W)))    
  })

  val coefFfs = Wire(UInt(vecLen.W))

  if(selEn) {
    coefFfs := io.coefFfs.get
  } else {
    val coefSel = VecInit(io.coefVec.bits.map(x => x.orR))
    val ffs = Module(new FindFirstSetNew(vecLen, false))
    ffs.io.in := coefSel.asTypeOf(UInt(vecLen.W))
    coefFfs := ffs.io.out
  }

  val qEn = if(comboLen < vecLen) true else false

  class MuxInBundle extends Bundle {
    val data = UInt(c.SYMB_WIDTH.W)
    val valid = UInt(1.W)
  }

  val combStage = for(i <- 0 until vecLen-1) yield Module(new GfPolyEvalHornerStage(c))
  val combVld = Wire(Vec(vecLen-1, Bool()))
  val combX = Wire(Vec(vecLen-1, UInt(c.SYMB_WIDTH.W)))

  val muxIn = Wire(Vec(vecLen-1, new MuxInBundle))

  combStage(0).io.x := io.x
  combStage(0).io.coef := io.coefVec.bits(1)
  combStage(0).io.prev := io.coefVec.bits(0)
  combVld(0) := io.coefVec.valid
  combX(0) := io.x

  muxIn(0).data := combStage(0).io.next
  muxIn(0).valid := combVld(0)

  for(i <- 1 until vecLen-1) {
    combStage(i).io.coef := io.coefVec.bits(i+1)
    if(i%comboLen == 0) {
      val qStage = Reg(UInt(c.SYMB_WIDTH.W))
      val qX = Reg(UInt(c.SYMB_WIDTH.W))      
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

  // combStage(0) utilize 0 and 1 terms of Poly
  // so coefFfs is truncated.
  val muxOut = Mux1H(coefFfs(vecLen-1,1), muxIn)

  io.evalValue.bits := muxOut.data
  io.evalValue.valid := muxOut.valid

}

class GfPolyEvalHornerStage(c: Config) extends Module {
  val io = IO(new Bundle {
    val x = Input(UInt(c.SYMB_WIDTH.W))
    val coef = Input(UInt(c.SYMB_WIDTH.W))
    val prev = Input(UInt(c.SYMB_WIDTH.W))
    val next = Output(UInt(c.SYMB_WIDTH.W))
  })

  val mult = c.gfMult(io.x, io.prev)
  io.next := mult ^ io.coef

}

class GfPolyEval(c: Config, vecLen: Int, selEn: Boolean=false) extends Module {
  val io = IO(new Bundle {
    val coefVec = Input(Valid(Vec(vecLen, UInt(c.SYMB_WIDTH.W))))
    val coefFfs = if(selEn) Some(Input(UInt(vecLen.W))) else None
    val x = Input(UInt(c.SYMB_WIDTH.W))
    val evalValue = Output(Valid(UInt(c.SYMB_WIDTH.W)))    
  })

  val stageOut = Wire(Vec(vecLen, UInt(c.SYMB_WIDTH.W)))
  for(i <- 0 until vecLen) {
    val xPower = c.gfPow(io.x, (vecLen-1-i).U)
    stageOut(i) := c.gfMult(io.coefVec.bits(i), xPower)
  }

  //val termsSumVec = (stageOut zip io.sAxisIf.bits.tkeep.asTypeOf(Vec(vecLen, Bool()))).map{case(a,b) => a & Fill(c.SYMB_WIDTH,b) }
  //io.evalValue.bits := termsSumVec.reduce(_^_)
  io.evalValue.bits := stageOut.reduce(_^_)
  io.evalValue.valid := io.coefVec.valid
}
