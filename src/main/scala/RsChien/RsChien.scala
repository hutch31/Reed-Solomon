package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

/////////////////////////////////////////
// RsChienGetBitPos 
/////////////////////////////////////////

// TODO: Add error when bitPos > tLen

class RsChienBitPosToNum extends Module with GfParams {
  val io = IO(new Bundle {
    val bitPos = Input(new BitPosIf)
    val posArray = Output(new NumPosIf)
  })

  val posOh = for(i <- 0 until tLen) yield Module(new RsChienPosOh())

  val posOhCapt = Reg(Vec(tLen, new PosBaseVld))
  val prePipe = Wire(Vec(tLen, new PosBaseLastVld))
  val postPipe = Wire(Vec(tLen-1, new PosBaseLastVld))
  val prePipeVld = Wire(Vec(tLen, Bool()))
  val postPipeVld = Wire(Vec(tLen-1, Bool()))

  val base = RegInit(UInt(symbWidth.W), 0.U)

  when(io.bitPos.valid){
    base := base + chienRootsPerCycle
  }.otherwise{
    base := 0
  }

  //////////////////////////////
  // Pipelining FFS logic
  //////////////////////////////

  val pipe = Module(new pipeline(new PosBaseLastVld, ffStepPosToNum, tLen, true))

  pipe.io.prePipe := prePipe
  pipe.io.prePipeVld.get := prePipeVld
  postPipe := pipe.io.postPipe
  postPipeVld := pipe.io.postPipeVld.get

  for(i <- 0 until tLen) {    
    if(i == 0) {
      when(io.bitPos.valid === 1) {
        posOh(i).io.bitPos := io.bitPos.pos
      }.otherwise{
        posOh(i).io.bitPos := 0.U
      }
      prePipe(i).pos     := posOh(i).io.lsbPosXor
      prePipe(i).base    := base
      prePipeVld(i)      := io.bitPos.valid
      prePipe(i).last    := io.bitPos.last
    } else {
      posOh(i).io.bitPos := postPipe(i-1).pos
      prePipe(i).pos     := posOh(i).io.lsbPosXor
      prePipe(i).base    := postPipe(i-1).base
      prePipeVld(i) := postPipeVld(i-1)
      prePipe(i).last    := postPipe(i-1).last
    }
    posOh(i).io.bypass := posOhCapt(i).valid
  }

  //////////////////////////////
  // Capture oh pos and base signals
  //////////////////////////////

  for(i <- 0 until tLen) {
    when(prePipeVld.asTypeOf(UInt(tLen.W)) =/= 0) {
      when(posOh(i).io.lsbPos =/= 0) {
        posOhCapt(i).valid := 1
        posOhCapt(i).pos := posOh(i).io.lsbPos
        posOhCapt(i).base := prePipe(i).base
      }
    }.otherwise{
      posOhCapt(i).valid := 0
    }
    io.posArray.pos(i) := nLen - 1 - ohToNum(posOhCapt(i).pos, posOhCapt(i).base)
  }

  io.posArray.sel := VecInit(posOhCapt.map(_.valid)).asTypeOf(UInt(tLen.W))

  io.posArray.valid := RegNext(prePipe(tLen-1).last)

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

  val ffs = Wire(UInt(chienRootsPerCycle.W))

  ffs := 0.U
  for(i <- (chienRootsPerCycle-1 to 0 by -1)) {
    when(io.bitPos(i) === 1) {
      ffs := (1 << i).U
    }
  }

  when(io.bypass === 1.U){
    io.lsbPos := 0.U
  }.otherwise {
    io.lsbPos := ffs
  }

  io.lsbPosXor := io.bitPos ^ io.lsbPos
  
}

/////////////////////////////////////////
// RsChien
/////////////////////////////////////////
class RsChien extends Module with GfParams{
  val io = IO(new Bundle {
    val errLocIf = Input(Valid(Vec(tLen+1, UInt(symbWidth.W))))   
    val posArray = Output(new NumPosIf)
  })

  val rsChienErrBitPos = Module(new RsChienErrBitPos)
  val rsChienBitPosToNum = Module(new RsChienBitPosToNum)

  rsChienErrBitPos.io.errLocIf := io.errLocIf
  rsChienBitPosToNum.io.bitPos := rsChienErrBitPos.io.bitPos
  io.posArray := rsChienBitPosToNum.io.posArray
}

//
// runMain Rs.GenChien
object GenChien extends App with GfParams{
  //ChiselStage.emitSystemVerilogFile(new GfPolyEvalHorner(tLen, ffStepPolyEval), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChienErrBitPos(), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChienBitPosToNum(), Array())
  ChiselStage.emitSystemVerilogFile(new RsChien(), Array())
}

/////////////////////////////////////////
// Pipelining
/////////////////////////////////////////
