package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class RsBm(c: Config) extends Module {
  val io = IO(new Bundle {
    val syndIf = Input(Valid(Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W))))
    val errLocIf = Output(Valid(new vecFfsIf(c.T_LEN+1, c.SYMB_WIDTH)))
  })

  val lenWidth = log2Ceil(c.REDUNDANCY)
  val rsBmStage = for(i <- 0 until c.bmTermsPerCycle) yield  Module(new RsBmStage(c, lenWidth))

  val syndInvVec = Wire(Vec(c.REDUNDANCY, Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W))))

  ///////////////////////////
  // Syndrome manipulation
  //
  // syndInvVec(0) = [synd(0),       0,       0,       0, 0, ... 0]
  // syndInvVec(1) = [synd(1), synd(0),       0,       0, 0, ... 0]
  // syndInvVec(2) = [synd(2), synd(1), synd(0),       0, 0, ... 0]
  // syndInvVec(3) = [synd(3), synd(2), synd(1), synd(0), 0, ... 0]
  ///////////////////////////

  for(rootIndx <- 0 until c.REDUNDANCY) {
    for(symbIndx <- 0 until c.T_LEN+1) {
      if(symbIndx <= rootIndx)
        syndInvVec(rootIndx)(symbIndx) := io.syndIf.bits(rootIndx-symbIndx)
      else
        syndInvVec(rootIndx)(symbIndx) := 0.U
    }
  }

  ///////////////////////////
  // Shift 
  ///////////////////////////

  val shiftMod = Module(new ShiftBundleMod(new ShiftUnit, width=c.REDUNDANCY, shiftVal=c.bmTermsPerCycle, interTermDelay=c.bmShiftLimit))

  class ShiftUnit extends Bundle {
    val syndInv = Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W))
    val iterI = UInt(lenWidth.W)
    val eop = Bool()
  }

  // Map inputs
  shiftMod.io.vecIn.valid := io.syndIf.valid

  for(i <- 0 until c.REDUNDANCY) {
    shiftMod.io.vecIn.bits(i).syndInv := syndInvVec(i)
    shiftMod.io.vecIn.bits(i).iterI := (1+i).U
    if(i == c.REDUNDANCY-1)
      shiftMod.io.vecIn.bits(i).eop := true.B
    else
      shiftMod.io.vecIn.bits(i).eop := false.B
  }

  ///////////////////////////
  // Regs stage
  ///////////////////////////

  val errLocQ = Reg(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  val errLocPiepEnQ = Reg(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))

  val errLocVldQ = RegInit(Bool(), false.B)
  val errLocLenQ = Reg(UInt(lenWidth.W))
  val auxBQ = Reg(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))

  val stageOut = Wire(Vec(c.bmTermsPerCycle, (Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))))
  val errLocVldStage = shiftMod.io.vecOut.bits.map(_.eop)

  val lastOutQ = RegNext(shiftMod.io.lastOut, false.B)
  val lastOut = Wire(Bool())
  
  // lastOut should be one cycle
  if(c.bmStagePipeEn){
    lastOut := ~lastOutQ & shiftMod.io.lastOut
  }else{
    lastOut := shiftMod.io.lastOut
  }

  errLocVldQ := errLocVldStage.reduce(_||_) && lastOut

  when(io.syndIf.valid) {
    errLocLenQ := 0.U
    for(i <- 0 until c.T_LEN+1) {
      if(i == 0) {
        errLocQ(i) := 1.U
        auxBQ(i) := 1.U
      } else {
        errLocQ(i) := 0.U
        auxBQ(i) := 0.U
      }
    }
  }.otherwise {
    auxBQ := rsBmStage(c.bmTermsPerCycle-1).io.auxBOut
    errLocLenQ := rsBmStage(c.bmTermsPerCycle-1).io.errLocLenOut
    when(lastOut) {
      if(c.bmTermsPerCycle == 1)
        errLocQ := stageOut(c.bmTermsPerCycle-1)
      else
        errLocQ := Mux1H(errLocVldStage, stageOut)
    }.otherwise {
        errLocQ := stageOut(c.bmTermsPerCycle-1)
    }
  }

  if(c.bmStagePipeEn){
    when(lastOutQ & shiftMod.io.lastOut){
      errLocPiepEnQ := stageOut(c.bmTermsPerCycle-1)
    }
  } else {
     when(lastOut){
       errLocPiepEnQ := stageOut(c.bmTermsPerCycle-1)
    }
  }

  ///////////////////////////
  // Combo stage
  ///////////////////////////

  val errLocLenVec = Reg(Vec(c.bmTermsPerCycle, UInt(lenWidth.W)))

  rsBmStage(0).io.iterI       := shiftMod.io.vecOut.bits(0).iterI
  rsBmStage(0).io.syndInvIn   := shiftMod.io.vecOut.bits(0).syndInv
  rsBmStage(0).io.errLocIn    := errLocQ
  rsBmStage(0).io.auxBIn      := auxBQ
  rsBmStage(0).io.errLocLenIn := errLocLenQ
  stageOut(0) := rsBmStage(0).io.errLocOut
  errLocLenVec(0) := errLocLenQ

  if(c.bmTermsPerCycle > 1) {
    for(i <- 1 until c.bmTermsPerCycle) {
      rsBmStage(i).io.iterI       := shiftMod.io.vecOut.bits(i).iterI
      rsBmStage(i).io.syndInvIn   := shiftMod.io.vecOut.bits(i).syndInv
      rsBmStage(i).io.errLocIn    := rsBmStage(i-1).io.errLocOut
      rsBmStage(i).io.auxBIn      := rsBmStage(i-1).io.auxBOut
      rsBmStage(i).io.errLocLenIn := rsBmStage(i-1).io.errLocLenOut
      stageOut(i)                 := rsBmStage(i).io.errLocOut
      errLocLenVec(i)             := rsBmStage(i-1).io.errLocLenOut
    }
  }

  val ffs = Module(new FindFirstSetNew(width=c.T_LEN+1, lsbFirst=false))
  ffs.io.in := VecInit(errLocQ.map(x => x.orR)).asTypeOf(UInt((c.T_LEN+1).W))

  if(c.bmStagePipeEn) {
    io.errLocIf.bits.vec := errLocPiepEnQ
    io.errLocIf.valid := RegNext(errLocVldQ, init = 0.U)
  } else {
    io.errLocIf.bits.vec := errLocPiepEnQ
    io.errLocIf.valid := errLocVldQ
  }

  io.errLocIf.bits.ffs := ffs.io.out

  /////////////////
  // Assert not ready
  /////////////////
  val notReadyAssrt = Module(new NotReadyAssrt(true))
  notReadyAssrt.io.start := io.syndIf.valid
  //notReadyAssrt.io.stop := io.errLocIf.valid
  notReadyAssrt.io.stop := lastOut

}

class RsBmStage(c: Config, lenWidth: Int) extends Module {
  val io = IO(new Bundle{
    val iterI = Input(UInt(lenWidth.W))
    val syndInvIn = Input(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
    val errLocIn = Input(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
    val auxBIn = Input(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
    val errLocLenIn = Input(UInt(lenWidth.W))
    val errLocOut = Output(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
    val auxBOut = Output(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))    
    val errLocLenOut = Output(UInt(lenWidth.W))
  })

  val syndInvVld = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))

  for(symbIndx <- 0 until c.T_LEN+1) {
    when(io.errLocLenIn < symbIndx.U) {
      syndInvVld(symbIndx) := 0.U
    }.otherwise{
      syndInvVld(symbIndx) := io.syndInvIn(symbIndx)
    }
  }

  val errLocLenPipe = Wire(UInt(lenWidth.W))
  val deltaPipe     = Wire(UInt(c.SYMB_WIDTH.W))
  val BxXPipe       = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  val errLocPipe    = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  val auxBPipe      = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))

  val deltaIntrm = (syndInvVld zip io.errLocIn).map{case(a,b) => c.gfMult(a,b)}
  val delta = deltaIntrm.reduce(_^_)
  val BxX = c.gfPolyMultX(io.auxBIn)

  // Pipelining BM stage
  if(c.bmStagePipeEn) {
    errLocLenPipe := RegNext(io.errLocLenIn)
    deltaPipe     := RegNext(delta)
    BxXPipe       := RegNext(BxX)
    errLocPipe    := RegNext(io.errLocIn)
    auxBPipe      := RegNext(io.auxBIn)
  } else {
    errLocLenPipe := io.errLocLenIn
    deltaPipe     := delta 
    BxXPipe       := BxX
    errLocPipe    := io.errLocIn      
    auxBPipe      := io.auxBIn
  }

  val deltaInv = c.gfInv(deltaPipe)


  val BxXxDelta = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  BxXxDelta := BxXPipe.map(c.gfMult(_, deltaPipe))

  val errLocLenx2 = errLocLenPipe << 1

  val errLocOut = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  val auxBOut = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  val errLocLenOut = Wire(UInt(lenWidth.W))

  when(deltaPipe =/= 0.U) {
    errLocOut := (errLocPipe zip BxXxDelta).map{case(a,b) => a ^ b}
    when(errLocLenx2 <= (io.iterI-1.U)) {
      errLocLenOut := io.iterI - errLocLenPipe
      auxBOut := (errLocPipe).map(c.gfMult(_, deltaInv))
    }.otherwise{
      errLocLenOut := errLocLenPipe
      auxBOut := c.gfPolyMultX(auxBPipe)
    }
  }.otherwise{
    errLocOut := errLocPipe
    errLocLenOut := errLocLenPipe
    auxBOut := c.gfPolyMultX(auxBPipe)
  }

  io.errLocOut := errLocOut
  io.auxBOut := auxBOut
  io.errLocLenOut := errLocLenOut
}

// runMain Rs.GenBm
object GenBm extends App {
  val config = JsonHandler.readConfig("/home/egorman44/chisel-lib/rs.json")
  ChiselStage.emitSystemVerilogFile(new RsBm(config), Array())
}

