package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

/////////////////////////////////////////
// RsChienBitPosToNum 
/////////////////////////////////////////

class RsChienBitPosToNum(c: Config) extends Module{
  val io = IO(new Bundle {
    val bitPos = Input(new BitPosIf(c.chienRootsPerCycle))
    val errPosIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
  })

  val base = RegInit(UInt(c.SYMB_WIDTH.W), 0.U)

  when(io.bitPos.valid){
    base := base + c.chienRootsPerCycle
  }.otherwise{
    base := 0
  }

  //////////////////////////////
  // Pipelining FFS logic
  //////////////////////////////
  class PosBaseVld() extends Bundle {
    val pos = UInt(c.chienRootsPerCycle.W)
    val base = UInt(c.SYMB_WIDTH.W)
    val valid = Bool()
  }

  val stageCapt = Reg(Vec(c.T_LEN, new PosBaseVld))
  val stageComb = for(i <- 0 until c.T_LEN) yield Module(new RsChienPosOh(c))
  val baseComb = Wire(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
  val validComb = Wire(Vec(c.T_LEN, Bool()))
  val lastComb = Wire(Vec(c.T_LEN, Bool()))

  dontTouch(validComb)

  when(io.bitPos.valid === 1) {
    stageComb(0).io.bitPos := io.bitPos.pos
  }.otherwise{
    stageComb(0).io.bitPos := 0.U
  }
  stageComb(0).io.bypass := stageCapt(0).valid

  baseComb(0)  := base
  validComb(0) := io.bitPos.valid
  lastComb(0)  := io.bitPos.last

  for(i <- 1 until c.T_LEN) {
    if(i % c.chienPosToNumComboLen == 0){
      val bitPosQ = Reg(UInt(c.chienRootsPerCycle.W))
      val baseQ = Reg(UInt(c.SYMB_WIDTH.W))
      val lastQ = Reg(Bool())
      val validQ = RegInit(Bool(), false.B)
      bitPosQ         := stageComb(i-1).io.lsbPosXor
      stageComb(i).io.bitPos := bitPosQ
      baseQ           := baseComb(i-1)
      baseComb(i)     := baseQ
      validQ          := validComb(i-1)
      validComb(i)    := validQ
      lastQ           := lastComb(i-1)
      lastComb(i)     := lastQ
    } else {
      stageComb(i).io.bitPos := stageComb(i-1).io.lsbPosXor
      baseComb(i)  := baseComb(i-1)
      validComb(i) := validComb(i-1)
      lastComb(i)  := lastComb(i-1)
    }
    stageComb(i).io.bypass := stageCapt(i).valid
  }

  //////////////////////////////
  // Capture oh pos and base signals
  //
  // Capture and store data till the next portion of the
  // data available. FFS should be captured separately since
  // stageCapt(i).valid is reseted when capture disabled.
  //////////////////////////////

  // Capture enabled only when data is valid in the pipe
  val captEn = validComb.asTypeOf(UInt(c.T_LEN.W)).orR
  dontTouch(captEn)
  val errPos = Wire(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))

  for(i <- 0 until c.T_LEN) {    
    when(captEn) {
      when(stageComb(i).io.lsbPos =/= 0) {
        stageCapt(i).valid := 1
        stageCapt(i).pos := stageComb(i).io.lsbPos
        stageCapt(i).base := baseComb(i)
      }
    }.otherwise{
      stageCapt(i).valid := 0
    }
    val baseArray = Wire(Vec(c.chienRootsPerCycle, UInt(c.SYMB_WIDTH.W)))
    for(k <- 0 until c.chienRootsPerCycle) {
      baseArray(k) := stageCapt(i).base + k.U
    }
    errPos(i) := c.N_LEN - 1 - Mux1H(stageCapt(i).pos, baseArray)
  }

  val captFfsQ = RegNext(next=lastComb(c.T_LEN-1), false.B)

  val stageValid = VecInit(stageCapt.map(_.valid))

  val ffsQ = RegInit(UInt(c.T_LEN.W), 0.U)
  val errPosQ = Reg(Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))
  
  when(captFfsQ) {
    ffsQ := stageValid.asTypeOf(UInt(c.T_LEN.W))
    errPosQ := (errPos zip stageValid).map{case(a,b) => a & Fill(c.SYMB_WIDTH,b)}
  }

  io.errPosIf.bits.vec := errPosQ
  io.errPosIf.bits.ffs := ffsQ
  io.errPosIf.valid := RegNext(next=captFfsQ, false.B)

}

/////////////////////////////////////////
// RsChienPosOh 
/////////////////////////////////////////

class RsChienPosOh(c: Config) extends Module{
  val io = IO(new Bundle {
    val bitPos = Input(UInt(c.chienRootsPerCycle.W))
    val bypass = Input(Bool())
    val lsbPos = Output(UInt(c.chienRootsPerCycle.W))
    val lsbPosXor = Output(UInt(c.chienRootsPerCycle.W))
  })

  val ffs = Module(new FindFirstSetNew(width=c.chienRootsPerCycle, lsbFirst=true))
  ffs.io.in := io.bitPos

  when(io.bypass === 1.U){
    io.lsbPos := 0.U
  }.otherwise {
    io.lsbPos := ffs.io.out
  }

  // Remove logical one in the most significant position of the vector
  io.lsbPosXor := io.bitPos ^ io.lsbPos

}
