package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsBm extends Module with GfParams {
  val io = IO(new Bundle {
    val syndIf = Input(Valid(Vec(redundancy, UInt(symbWidth.W))))
    val errLocIf = Output(Valid(Vec(tLen+1, UInt(symbWidth.W))))
  })
  val lenWidth = log2Ceil(redundancy)
  val rsBmStage = for(i <- 0 until numOfSymbBm) yield  Module(new RsBmStage(lenWidth))

  val syndInvVec = Wire(Vec(redundancy, Vec(tLen+1, UInt(symbWidth.W))))

  ///////////////////////////
  // Syndrome manipulation
  ///////////////////////////

  for(rootIndx <- 0 until redundancy) {
    for(symbIndx <- 0 until tLen+1) {
      if(symbIndx <= rootIndx)
        syndInvVec(rootIndx)(symbIndx) := io.syndIf.bits(rootIndx-symbIndx)
      else
        syndInvVec(rootIndx)(symbIndx) := 0.U
    }
  }

  ///////////////////////////
  // Shift 
  ///////////////////////////

  val shiftMod = Module(new ShiftBundleMod(new ShiftUnit, redundancy, numOfSymbBm))

  class ShiftUnit extends Bundle {
    val syndInv = Vec(tLen+1, UInt(symbWidth.W))
    val iterI = UInt(lenWidth.W)
    val eop = Bool()
  }

  // Map inputs
  shiftMod.io.vecIn.valid := io.syndIf.valid

  for(i <- 0 until redundancy) {
    shiftMod.io.vecIn.bits(i).syndInv := syndInvVec(i)
    shiftMod.io.vecIn.bits(i).iterI := (1+i).U
    if(i == redundancy-1)
      shiftMod.io.vecIn.bits(i).eop := true.B
    else
      shiftMod.io.vecIn.bits(i).eop := false.B
  }

  ///////////////////////////
  // Regs stage
  ///////////////////////////

  val errLocQ = Reg(Vec(tLen+1, UInt(symbWidth.W)))
  val errLocVldQ = RegInit(Bool(), 0.U)
  val errLocLenQ = Reg(UInt(lenWidth.W))
  val auxBQ = Reg(Vec(tLen+1, UInt(symbWidth.W)))

  val stageOut = Wire(Vec(numOfSymbBm, (Vec(tLen+1, UInt(symbWidth.W)))))
  val errLocVldStage = shiftMod.io.vecOut.bits.map(_.eop)

  errLocVldQ := errLocVldStage.reduce(_||_) && shiftMod.io.lastOut

  when(io.syndIf.valid) {
    errLocLenQ := 0.U
    for(i <- 0 until tLen+1) {
      if(i == 0) {
        errLocQ(i) := 1.U
        auxBQ(i) := 1.U
      } else {
        errLocQ(i) := 0.U
        auxBQ(i) := 0.U
      }
    }
  }.otherwise {
    //errLocQ := rsBmStage(numOfSymbBm-1).io.errLocOut
    auxBQ := rsBmStage(numOfSymbBm-1).io.auxBOut
    errLocLenQ := rsBmStage(numOfSymbBm-1).io.errLocLenOut
    when(shiftMod.io.lastOut) {
      if(numOfSymbBm == 1)
        errLocQ := stageOut(numOfSymbBm-1)
      else
        errLocQ := Mux1H(errLocVldStage, stageOut)
    }.otherwise {
        errLocQ := stageOut(numOfSymbBm-1)
    }
  }

  ///////////////////////////
  // Combo stage
  ///////////////////////////

  val errLocLenVec = Reg(Vec(numOfSymbBm, UInt(lenWidth.W)))

  rsBmStage(0).io.iterI       := shiftMod.io.vecOut.bits(0).iterI
  rsBmStage(0).io.syndInvIn   := shiftMod.io.vecOut.bits(0).syndInv
  rsBmStage(0).io.errLocIn    := errLocQ
  rsBmStage(0).io.auxBIn      := auxBQ
  rsBmStage(0).io.errLocLenIn := errLocLenQ
  stageOut(0) := rsBmStage(0).io.errLocOut
  errLocLenVec(0) := errLocLenQ

  if(numOfSymbBm > 1) {
    for(i <- 1 until numOfSymbBm) {
      rsBmStage(i).io.iterI       := shiftMod.io.vecOut.bits(i).iterI
      rsBmStage(i).io.syndInvIn   := shiftMod.io.vecOut.bits(i).syndInv
      rsBmStage(i).io.errLocIn    := rsBmStage(i-1).io.errLocOut
      rsBmStage(i).io.auxBIn      := rsBmStage(i-1).io.auxBOut
      rsBmStage(i).io.errLocLenIn := rsBmStage(i-1).io.errLocLenOut
      stageOut(i)     := rsBmStage(i).io.errLocOut
      errLocLenVec(i) := rsBmStage(i-1).io.errLocLenOut
    }
  }

  // TODO: connect proper output
  io.errLocIf.bits := errLocQ
  io.errLocIf.valid := errLocVldQ
  
}

class RsBmStage(lenWidth: Int) extends Module with GfParams {
  val io = IO(new Bundle{
    val iterI = Input(UInt(lenWidth.W))
    val syndInvIn = Input(Vec(tLen+1, UInt(symbWidth.W)))
    val errLocIn = Input(Vec(tLen+1, UInt(symbWidth.W)))
    val auxBIn = Input(Vec(tLen+1, UInt(symbWidth.W)))
    val errLocLenIn = Input(UInt(lenWidth.W))
    val errLocOut = Output(Vec(tLen+1, UInt(symbWidth.W)))
    val auxBOut = Output(Vec(tLen+1, UInt(symbWidth.W)))    
    val errLocLenOut = Output(UInt(lenWidth.W))
  })

  val syndInvVld = Wire(Vec(tLen+1, UInt(symbWidth.W)))

  for(symbIndx <- 0 until tLen+1) {
    when(io.errLocLenIn < symbIndx.U) {
      syndInvVld(symbIndx) := 0.U
    }.otherwise{
      syndInvVld(symbIndx) := io.syndInvIn(symbIndx)
    }
  }

  val deltaIntrm = (syndInvVld zip io.errLocIn).map{case(a,b) => gfMult(a,b)}

  //val delta = Reg(UInt(symbWidth.W))
  val delta = deltaIntrm.reduce(_^_)
  //delta := deltaIntrm.reduce(_^_)
  val deltaInv = gfInv(delta)

  val BxX = gfPolyMultX(io.auxBIn)

  val BxXxDelta = Wire(Vec(tLen+1, UInt(symbWidth.W)))
  BxXxDelta := (BxX).map(gfMult(_, delta))

  val errLocLenx2 = io.errLocLenIn << 1

  val errLocOut = Wire(Vec(tLen+1, UInt(symbWidth.W)))
  val auxBOut = Wire(Vec(tLen+1, UInt(symbWidth.W)))
  val errLocLenOut = Wire(UInt(lenWidth.W))

  when(delta =/= 0.U) {
    errLocOut := (io.errLocIn zip BxXxDelta).map{case(a,b) => a ^ b}
    when(errLocLenx2 <= (io.iterI-1.U)) {
      errLocLenOut := io.iterI - io.errLocLenIn
      auxBOut := (io.errLocIn).map(gfMult(_, deltaInv))
    }.otherwise{
      errLocLenOut := io.errLocLenIn
      auxBOut := gfPolyMultX(io.auxBIn)
    }
  }.otherwise{
    errLocOut := io.errLocIn
    errLocLenOut := io.errLocLenIn
    auxBOut := gfPolyMultX(io.auxBIn)
  }

  io.errLocOut := errLocOut
  io.auxBOut := auxBOut
  io.errLocLenOut := errLocLenOut

}

object GenBm extends App {
  //ChiselStage.emitSystemVerilogFile(new RsBmStage(4), Array())
  ChiselStage.emitSystemVerilogFile(new RsBm(), Array())
}

