package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsBm extends Module with GfParams {
  val io = IO(new Bundle {
    val syndIf = Input(Valid(Vec(redundancy, UInt(symbWidth.W))))
    val errLoc = Output(Vec(tLen+1, UInt(symbWidth.W)))
  })
  val lenWidth = log2Ceil(redundancy)
  val rsBmStage = for(i <- 0 until numOfSymbBm) yield  Module(new RsBmStage(lenWidth))

  val syndInvVec = Wire(Vec(redundancy, Vec(tLen+1, UInt(symbWidth.W))))

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
    //val ffs = Bool()
  }

  // Map inputs
  shiftMod.io.vecIn.valid := io.syndIf.valid

  for(i <- 0 until redundancy) {
    shiftMod.io.vecIn.bits(i).syndInv := syndInvVec(i)
    shiftMod.io.vecIn.bits(i).iterI := i.U
  }

  ///////////////////////////
  // Regs stage
  ///////////////////////////

  val errLocQ = Reg(Vec(tLen+1, UInt(symbWidth.W)))
  val errLocLenQ = Reg(UInt(lenWidth.W))
  val auxBQ = Reg(Vec(tLen+1, UInt(symbWidth.W)))

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
    errLocQ := rsBmStage(numOfSymbBm-1).io.errLocOut
    auxBQ := rsBmStage(numOfSymbBm-1).io.auxBOut
    errLocLenQ := rsBmStage(numOfSymbBm-1).io.errLocLenOut
  }

  ///////////////////////////
  // Combo stage
  ///////////////////////////

  val errLocLenVec = Reg(Vec(numOfSymbBm, UInt(lenWidth.W)))
  val syndInvVecVld = Wire(Vec(numOfSymbBm, Vec(tLen+1, UInt(symbWidth.W))))

  for(stageIndx <- 0 until numOfSymbBm) {
    for(symbIndx <- 0 until tLen+1) {
      when(errLocLenVec(stageIndx) < symbIndx.U) {
        syndInvVecVld(stageIndx)(symbIndx) := shiftMod.io.vecOut.bits(stageIndx).syndInv(symbIndx)
      }.otherwise {
        syndInvVecVld(stageIndx)(symbIndx) := 0.U
      }
    }
  }

  rsBmStage(0).io.iterI       := shiftMod.io.vecOut.bits(0).iterI
  rsBmStage(0).io.syndInvIn   := shiftMod.io.vecOut.bits(0).syndInv
  rsBmStage(0).io.errLocIn    := errLocQ
  rsBmStage(0).io.auxBIn      := auxBQ
  rsBmStage(0).io.errLocLenIn := errLocLenQ

  errLocLenVec(0) := errLocLenQ

  if(numOfSymbBm > 1) {
    for(i <- 0 until numOfSymbBm) {
      rsBmStage(i).io.iterI       := shiftMod.io.vecOut.bits(i).iterI
      rsBmStage(i).io.syndInvIn   := syndInvVecVld(i)
      rsBmStage(i).io.errLocIn    := rsBmStage(i-1).io.errLocOut
      rsBmStage(i).io.auxBIn      := rsBmStage(i-1).io.auxBOut
      rsBmStage(i).io.errLocLenIn := rsBmStage(i-1).io.errLocLenOut
      errLocLenVec(i) := rsBmStage(i-1).io.errLocLenOut
    }
  }

  // TODO: connect proper output
  io.errLoc := rsBmStage(numOfSymbBm-1).io.errLocOut
  
}

class RsBmStage(lenWidth: Int) extends Module with GfParams {
  val io = IO(new Bundle{
    val iterI = Input(UInt(lenWidth.W))
    val syndInvIn = Input(Vec(tLen+1, UInt(symbWidth.W)))
    val errLocIn = Input(Vec(tLen+1, UInt(symbWidth.W)))
    val errLocOut = Output(Vec(tLen+1, UInt(symbWidth.W)))
    val auxBIn = Input(Vec(tLen+1, UInt(symbWidth.W)))
    val auxBOut = Output(Vec(tLen+1, UInt(symbWidth.W)))
    val errLocLenIn = Input(UInt(lenWidth.W))
    val errLocLenOut = Output(UInt(lenWidth.W))

    // TODO: delete me
    val deltaInv = Output(UInt(symbWidth.W))
    val BxX = Output(Vec(tLen+1, UInt(symbWidth.W)))    
  })

  // Find delta
  // gfPolyMult()
  val deltaIntrm = (io.syndInvIn zip io.errLocIn).map{case(a,b) => gfMult(a,b)}
  val delta = deltaIntrm.reduce(_^_)
  val deltaInv = gfInv(delta)

  io.deltaInv := deltaInv

  val BxX = gfPolyMultX(io.auxBIn)
  val BxXxDelta = (BxX).map(gfMult(_, delta))
  io.BxX := BxXxDelta

  val errLocLenx2 = io.errLocLenIn << 1

  when(delta =/= 0.U) {
    io.errLocOut := (io.errLocIn zip BxXxDelta).map{case(a,b) => a ^ b}
    when(errLocLenx2 <= io.iterI) {
      io.errLocLenOut := io.iterI - io.errLocLenIn
      io.auxBOut := (io.errLocIn).map(gfMult(_, deltaInv))
    }.otherwise{
      io.errLocLenOut := io.errLocLenIn
      io.auxBOut := gfPolyMultX(io.auxBIn)
    }
  }.otherwise{
    io.errLocOut := io.errLocIn
    io.errLocLenOut := io.errLocLenIn
    io.auxBOut := gfPolyMultX(io.auxBIn)
  }

}

// runmain Rs.GenBm

object GenBm extends App {
  //ChiselStage.emitSystemVerilogFile(new RsBmStage(4), Array())
  ChiselStage.emitSystemVerilogFile(new RsBm(), Array())
}

