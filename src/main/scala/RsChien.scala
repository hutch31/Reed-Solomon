package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

class GfPolyEval extends Module with GfParams {
  val io = IO(new Bundle {
    val errLocatorIf = Input(new ErrLocatorBundle)
    val inSymb = Input(Valid(UInt(symbWidth.W)))
    val evalValue = Output(Valid(UInt(symbWidth.W)))
  })

  class SymbXorVld extends Bundle {
    val xor = UInt(symbWidth.W)
    val symb = UInt(symbWidth.W)
  }

  val gfMultIntrm = Wire(Vec(tLen, UInt(symbWidth.W)))

  val prePipe = Wire(Vec(tLen, new SymbXorVld))
  val postPipe = Wire(Vec(tLen-1, new SymbXorVld))
  val prePipeVld = Wire(Vec(tLen, Bool()))
  val postPipeVld = Wire(Vec(tLen-1, Bool()))

  for(i <- 0 until tLen) {
    if(i == 0) {
      gfMultIntrm(i) := gfMult(1, io.inSymb.bits)
      prePipeVld(i) := io.inSymb.valid
      prePipe(i).symb := io.inSymb.bits
    }else {
      gfMultIntrm(i) := gfMult(postPipe(i-1).xor, postPipe(i-1).symb)
      prePipeVld(i) := postPipeVld(i-1)
      prePipe(i).symb := postPipe(i-1).symb
    }
    prePipe(i).xor := gfMultIntrm(i) ^ io.errLocatorIf.errLocator(i)
  }

  /////////////////////////////////////////
  // Pipeline the comb logic if requred
  /////////////////////////////////////////

  val pipe = Module(new pipeline(new SymbXorVld, ffStepPolyEval, tLen, true))

  pipe.io.prePipe := prePipe
  pipe.io.prePipeVld.get := prePipeVld
  postPipe := pipe.io.postPipe
  postPipeVld := pipe.io.postPipeVld.get

  // Add registers
  val prioEnvOut = Wire(UInt(log2Ceil(tLen).W))

  prioEnvOut := 0.U
  for(i <- (0 until tLen)) {
    when(io.errLocatorIf.errLocatorSel(i) === 1) {
      prioEnvOut := i.U
    }
  }

  io.evalValue.valid := prePipeVld(prioEnvOut)
  io.evalValue.bits := prePipe(prioEnvOut).xor
  

}

/////////////////////////////////////////
// RsChienGetBitPos 
/////////////////////////////////////////

class RsChienGetBitPos extends Module with GfParams {
  val io = IO(new Bundle {
    val errLocatorIf = Input(Valid(new ErrLocatorBundle()))
    val bitPos = Output(new BitPosIf)
  })

  val polyEval = for(i <- 0 until chienRootsPerCycle) yield Module(new GfPolyEval())
  val base = Wire(Vec(chienRootsPerCycle, UInt(symbWidth.W)))
  val roots = Wire(Valid(Vec(chienRootsPerCycle, UInt(symbWidth.W))))
  val evalValue = Wire(Vec(chienRootsPerCycle, UInt(symbWidth.W)))

  val bitPos = Reg(Vec(chienRootsPerCycle, Bool() ))
  val bitPosVld = RegInit(Bool(), 0.U)

  for(i <- 0 until chienRootsPerCycle) {
    base(i) := i.U
  }

  if(chienRootsPerCycle == 1) {
    roots.bits := base
    roots.valid := io.errLocatorIf.valid
  } else {

    val cntr = RegInit(UInt(log2Ceil(chienCyclesNum).W), 0.U)
    val offset = RegInit(UInt(symbWidth.W), 0.U)

    when(io.errLocatorIf.valid === 1.U) {
      cntr := cntr + 1.U
      offset := offset + chienRootsPerCycle
    }.elsewhen(cntr =/= 0.U) {
      when(cntr =/= (chienCyclesNum-1).U) {
        cntr := cntr + 1.U
        offset := offset + chienRootsPerCycle
      }.otherwise {
        cntr := 0.U
        offset := 0.U
      }
    }

    for(i <- 0 until chienRootsPerCycle) {
      roots.bits(i) := alphaToSymb(base(i) + offset)
    }
    roots.valid := io.errLocatorIf.valid | (cntr =/= 0)

  }

  // Connect GfPolyEval
  for(i <- 0 until chienRootsPerCycle) {
    // Is it possible to use <> here to connect bundles 
    polyEval(i).io.errLocatorIf.errLocator := io.errLocatorIf.bits.errLocator
    polyEval(i).io.errLocatorIf.errLocatorSel := io.errLocatorIf.bits.errLocatorSel
    polyEval(i).io.inSymb.bits := roots.bits(i)
    polyEval(i).io.inSymb.valid := roots.valid
    evalValue(i) := polyEval(i).io.evalValue.bits
    when(evalValue(i) === 0.U) {
      bitPos(i) := 1.U
    } otherwise {
      bitPos(i) := 0.U
    }
  }

  // We can use any Valid here, so take (0)
  bitPosVld := polyEval(0).io.evalValue.valid

  val lastCycle = Wire(Bool())
  when(polyEval(0).io.evalValue.valid === 0.U & bitPosVld === 1.U){
    lastCycle := 1
    io.bitPos.last := 1
  }.otherwise{
    lastCycle := 0
    io.bitPos.last := 0
  }

  io.bitPos.valid := bitPosVld

  when(lastCycle & chienRootsPerCycle =/= 0.U) {
    // Nulify bits that is not valid.
    io.bitPos.pos := bitPos.asTypeOf(UInt(chienRootsPerCycle.W)) & chienNonValid
  }.otherwise{
    io.bitPos.pos := bitPos.asTypeOf(UInt(chienRootsPerCycle.W))
  }
  
}

/////////////////////////////////////////
// RsChienGetBitPos 
/////////////////////////////////////////

// TODO: Add error when bitPos > tLen

class RsChienBitPosToNum extends Module with GfParams {
  val io = IO(new Bundle {
    val bitPos = Input(new BitPosIf)
    val numArray = Output(new NumPosIf)
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
    io.numArray.sel(i) := posOhCapt(i).valid
    io.numArray.pos(i) := nLen - 1 - ohToNum(posOhCapt(i).pos, posOhCapt(i).base)
  }

  io.numArray.valid := RegNext(prePipe(tLen-1).last)

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
    val errLocatorIf = Input(Valid(new ErrLocatorBundle()))
    val numArray = Output(new NumPosIf)
  })

  val rsChienGetBitPos = Module(new RsChienGetBitPos)
  val rsChienBitPosToNum = Module(new RsChienBitPosToNum)

  rsChienGetBitPos.io.errLocatorIf := io.errLocatorIf
  rsChienBitPosToNum.io.bitPos := rsChienGetBitPos.io.bitPos
  io.numArray := rsChienBitPosToNum.io.numArray
}

//
// runMain Rs.GenTest
object GenTest extends App {
  //ChiselStage.emitSystemVerilogFile(new GfPolyEval(), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChienGetBitPos(), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChienBitPosToNum(), Array())
  ChiselStage.emitSystemVerilogFile(new RsChien(), Array())
}

/////////////////////////////////////////
// Pipelining
/////////////////////////////////////////
