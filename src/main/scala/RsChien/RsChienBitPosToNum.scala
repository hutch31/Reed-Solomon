package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

/////////////////////////////////////////
// RsChienBitPosToNum 
/////////////////////////////////////////

class RsChienBitPosToNum extends Module with GfParams {
  val io = IO(new Bundle {
    val bitPos = Input(new BitPosIf)
    val errPosIf = Output(Valid(new vecFfsIf(tLen)))
  })

  val base = RegInit(UInt(symbWidth.W), 0.U)

  when(io.bitPos.valid){
    base := base + chienRootsPerCycle
  }.otherwise{
    base := 0
  }

  //////////////////////////////
  // Pipelining FFS logic
  //////////////////////////////
  val stageCapt = Reg(Vec(tLen, new PosBaseVld))
  val stageComb = for(i <- 0 until tLen) yield Module(new RsChienPosOh())
  val baseComb = Wire(Vec(tLen, UInt(symbWidth.W)))
  val validComb = Wire(Vec(tLen, Bool()))
  val lastComb = Wire(Vec(tLen, Bool()))

  when(io.bitPos.valid === 1) {
    stageComb(0).io.bitPos := io.bitPos.pos
  }.otherwise{
    stageComb(0).io.bitPos := 0.U
  }
  stageComb(0).io.bypass := stageCapt(0).valid

  baseComb(0)  := base
  validComb(0) := io.bitPos.valid
  lastComb(0)  := io.bitPos.last

  for(i <- 1 until tLen) {
    if(i%chienPosToNumComboLen == 0){
      val bitPosQ = Reg(UInt(chienRootsPerCycle.W))
      val baseQ = Reg(UInt(symbWidth.W))
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
  //////////////////////////////

  // Capture enabled only when data is valid in the pipe
  val captEn = validComb.asTypeOf(UInt(tLen.W)).orR
  
  for(i <- 0 until tLen) {    
    when(captEn) {
      when(stageComb(i).io.lsbPos =/= 0) {
        stageCapt(i).valid := 1
        stageCapt(i).pos := stageComb(i).io.lsbPos
        stageCapt(i).base := baseComb(i)
      }
    }.otherwise{
      stageCapt(i).valid := 0
    }
    val baseArray = Wire(Vec(chienRootsPerCycle, UInt(symbWidth.W)))
    for(k <- 0 until chienRootsPerCycle) {
      baseArray(k) := stageCapt(i).base + k.U
    }
    io.errPosIf.bits.vec(i) := nLen - 1 - Mux1H(stageCapt(i).pos, baseArray)
  }

  val ffs = Module(new FindFirstSetNew(width=tLen, lsbFirst=false))
  ffs.io.in := VecInit(stageCapt.map(_.valid)).asTypeOf(UInt(tLen.W))

  io.errPosIf.bits.ffs := ffs.io.out

  io.errPosIf.valid := RegNext(lastComb(tLen-1))

}

/////////////////////////////////////////
// RsChienPosOh 
/////////////////////////////////////////

class RsChienPosOh extends Module with GfParams {
  val io = IO(new Bundle {
    val bitPos = Input(UInt(chienRootsPerCycle.W))
    val bypass = Input(Bool())
    val lsbPos = Output(UInt(chienRootsPerCycle.W))
    val lsbPosXor = Output(UInt(chienRootsPerCycle.W))
  })

  val ffs = Module(new FindFirstSetNew(width=chienRootsPerCycle, lsbFirst=true))
  ffs.io.in := io.bitPos

  when(io.bypass === 1.U){
    io.lsbPos := 0.U
  }.otherwise {
    io.lsbPos := ffs.io.out
  }

  io.lsbPosXor := io.bitPos ^ io.lsbPos

}
