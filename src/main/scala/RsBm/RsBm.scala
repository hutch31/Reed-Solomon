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
  // syndInvVec(1) = [synd(1), symd(0),       0,       0, 0, ... 0]
  // syndInvVec(2) = [synd(2), synd(1), symd(0),       0, 0, ... 0]
  // syndInvVec(3) = [synd(3), synd(2), symd(1), synd(0), 0, ... 0]
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

  val shiftMod = Module(new ShiftBundleMod(new ShiftUnit, c.REDUNDANCY, c.bmTermsPerCycle))

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
  val errLocVldQ = RegInit(Bool(), 0.U)
  val errLocLenQ = Reg(UInt(lenWidth.W))
  val auxBQ = Reg(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))

  val stageOut = Wire(Vec(c.bmTermsPerCycle, (Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))))
  val errLocVldStage = shiftMod.io.vecOut.bits.map(_.eop)

  errLocVldQ := errLocVldStage.reduce(_||_) && shiftMod.io.lastOut

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
    //errLocQ := rsBmStage(c.bmTermsPerCycle-1).io.errLocOut
    auxBQ := rsBmStage(c.bmTermsPerCycle-1).io.auxBOut
    errLocLenQ := rsBmStage(c.bmTermsPerCycle-1).io.errLocLenOut
    when(shiftMod.io.lastOut) {
      if(c.bmTermsPerCycle == 1)
        errLocQ := stageOut(c.bmTermsPerCycle-1)
      else
        errLocQ := Mux1H(errLocVldStage, stageOut)
    }.otherwise {
        errLocQ := stageOut(c.bmTermsPerCycle-1)
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
      stageOut(i)     := rsBmStage(i).io.errLocOut
      errLocLenVec(i) := rsBmStage(i-1).io.errLocLenOut
    }
  }

  val ffs = Module(new FindFirstSetNew(width=c.T_LEN+1, lsbFirst=false))
  ffs.io.in := VecInit(errLocQ.map(x => x.orR)).asTypeOf(UInt((c.T_LEN+1).W))
  // TODO: connect proper output
  io.errLocIf.bits.vec := errLocQ
  io.errLocIf.bits.ffs := ffs.io.out
  io.errLocIf.valid := errLocVldQ
  
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

  val deltaIntrm = (syndInvVld zip io.errLocIn).map{case(a,b) => c.gfMult(a,b)}

  //val delta = Reg(UInt(c.SYMB_WIDTH.W))
  val delta = deltaIntrm.reduce(_^_)
  //delta := deltaIntrm.reduce(_^_)
  val deltaInv = c.gfInv(delta)

  val BxX = c.gfPolyMultX(io.auxBIn)

  val BxXxDelta = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  BxXxDelta := (BxX).map(c.gfMult(_, delta))

  val errLocLenx2 = io.errLocLenIn << 1

  val errLocOut = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  val auxBOut = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  val errLocLenOut = Wire(UInt(lenWidth.W))

  when(delta =/= 0.U) {
    errLocOut := (io.errLocIn zip BxXxDelta).map{case(a,b) => a ^ b}
    when(errLocLenx2 <= (io.iterI-1.U)) {
      errLocLenOut := io.iterI - io.errLocLenIn
      auxBOut := (io.errLocIn).map(c.gfMult(_, deltaInv))
    }.otherwise{
      errLocLenOut := io.errLocLenIn
      auxBOut := c.gfPolyMultX(io.auxBIn)
    }
  }.otherwise{
    errLocOut := io.errLocIn
    errLocLenOut := io.errLocLenIn
    auxBOut := c.gfPolyMultX(io.auxBIn)
  }

  io.errLocOut := errLocOut
  io.auxBOut := auxBOut
  io.errLocLenOut := errLocLenOut

}

// runMain Rs.GenBm
object GenBm extends App {
  val config = JsonReader.readConfig("/home/egorman44/chisel-lib/rs.json")
  ChiselStage.emitSystemVerilogFile(new RsBm(config), Array())
}

