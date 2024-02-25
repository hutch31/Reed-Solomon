package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

class GfPolyEval extends Module with GfParams {
  val io = IO(new Bundle {
    //val errLocator = Input(Vec(tLen, UInt(symbWidth.W)))
    //val errLocatorSel = Input(UInt(tLen.W))
    val errLocatorIf = Input(new ErrLocatorBundle)
    val inSymb = Input(Valid(UInt(symbWidth.W)))
    val evalValue = Output(Valid(UInt(symbWidth.W)))
  })

  class SymbXorVld extends Bundle {
    val xor = UInt(symbWidth.W)
    val symb = UInt(symbWidth.W)
    val valid = Bool()
  }

  val gfMultIntrm = Wire(Vec(tLen, UInt(symbWidth.W)))

  val prePipe = Wire(Vec(tLen, new SymbXorVld))
  val postPipe = Wire(Vec(tLen-1, new SymbXorVld))

  for(i <- 0 until tLen) {
    if(i == 0) {
      gfMultIntrm(i) := gfMult(1, io.inSymb.bits)
      prePipe(i).valid := io.inSymb.valid
      prePipe(i).symb := io.inSymb.bits
    }else {
      gfMultIntrm(i) := gfMult(postPipe(i-1).xor, postPipe(i-1).symb)
      prePipe(i).valid := postPipe(i-1).valid
      prePipe(i).symb := postPipe(i-1).symb
    }
    prePipe(i).xor := gfMultIntrm(i) ^ io.errLocatorIf.errLocator(i)
  }

  /////////////////////////////////////////
  // Pipeline the comb logic if requred
  /////////////////////////////////////////

  // TODO: add reset for VLD
  //val pipeXorQ = Reg(Vec(ffNumPolyEVal, UInt(symbWidth.W)))
  //val pipeSymbQ = Reg(Vec(ffNumPolyEVal, UInt(symbWidth.W)))
  //val pipeVldQ = RegInit(0.U(ffNumPolyEVal.W))
  //
  //for(i <- 0 until ffNumPolyEVal) {
  //  pipeXorQ(i) := prePipe((i*ffStepPolyEval)+ffStepPolyEval-1).xor
  //  pipeSymbQ(i) := prePipe((i*ffStepPolyEval)+ffStepPolyEval-1).symb
  //  pipeVldQ(i) := prePipe((i*ffStepPolyEval)+ffStepPolyEval-1).valid
  //  postPipe((i*ffStepPolyEval)+ffStepPolyEval-1).xor := pipeXorQ(i)
  //  postPipe((i*ffStepPolyEval)+ffStepPolyEval-1).symb := pipeSymbQ(i)
  //  postPipe((i*ffStepPolyEval)+ffStepPolyEval-1).valid := pipeVldQ(i)
  //}

  val pipeQ = Reg(Vec(tLen, new SymbXorVld))
  
  for(i <- 0 until ffNumPolyEVal) {
    pipeQ(i) := prePipe((i*ffStepPolyEval)+ffStepPolyEval-1)
    postPipe((i*ffStepPolyEval)+ffStepPolyEval-1) := pipeQ(i)
  }

  for(i <- 0 until tLen) {
    if((i+1) % ffStepPolyEval != 0)
      postPipe(i) := prePipe(i)
  }

  // Add registers
  val prioEnvOut = Wire(UInt(log2Ceil(tLen).W))

  prioEnvOut := 0.U
  for(i <- (0 until tLen)) {
    when(io.errLocatorIf.errLocatorSel(i) === 1) {
      prioEnvOut := i.U
    }
  }

  io.evalValue.valid := prePipe(prioEnvOut).valid
  io.evalValue.bits := prePipe(prioEnvOut).xor
  

}

/////////////////////////////////////////
// RsChienGetBitPos 
/////////////////////////////////////////

class RsChienGetBitPos extends Module with GfParams {
  val io = IO(new Bundle {
    val errLocatorIf = Input(Valid(new ErrLocatorBundle()))
    val bitPos = Output(Valid(UInt(chienRootsPerCycle.W)))
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
    } .elsewhen(cntr =/= 0.U) {
      when(cntr =/= (chienCyclesNum-1).U) {
        cntr := cntr + 1.U
        offset := offset + chienRootsPerCycle
      } .otherwise {
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

  //
  val lastCycle = Wire(Bool())
  when(polyEval(0).io.evalValue.valid === 0.U & bitPosVld === 1.U){
    lastCycle := 1
  }.otherwise{
    lastCycle := 0
  }

  io.bitPos.valid := bitPosVld

  when(lastCycle & chienRootsPerCycle =/= 0.U) {
    // Nulify bits that is not valid.
    io.bitPos.bits := bitPos.asTypeOf(UInt(chienRootsPerCycle.W)) & chienNonValid
  }.otherwise{
    io.bitPos.bits := bitPos.asTypeOf(UInt(chienRootsPerCycle.W))
  }
  
}

/////////////////////////////////////////
// RsChienGetBitPos 
/////////////////////////////////////////

class RsChienBitPosToNum extends Module with GfParams {
  val io = IO(new Bundle {
    val bitPos = Input(new BitPosIf)
    val numArray = Output(new NumPosIf)
  })

  val posOh = for(i <- 0 until tLen) yield Module(new RsChienPosOh())

  val posOhCapt = Reg(Vec(tLen, new PosBaseVld))
  val prePipe = Wire(Vec(tLen, new PosBaseLastVld))
  val postPipe = Wire(Vec(tLen-1, new PosBaseLastVld))
  val base = RegInit(UInt(symbWidth.W), 0.U)
  val prePipeVld = Wire(Vec(tLen, Bool()))
  
  //////////////////////////////
  // Pipelining FFS logic
  //////////////////////////////

  when(io.bitPos.valid){
    base := base + chienRootsPerCycle
  }.otherwise{
    base := 0
  }

  for(i <- 0 until tLen) {    
    if(i == 0) {
      when(io.bitPos.valid === 1) {
        posOh(i).io.bitPos := io.bitPos.pos
      }.otherwise{
        posOh(i).io.bitPos := 0.U
      }
      prePipe(i).pos     := posOh(i).io.lsbPosXor
      prePipe(i).base    := base
      prePipe(i).valid   := io.bitPos.valid
      prePipe(i).last    := io.bitPos.last
    } else {
      posOh(i).io.bitPos := postPipe(i-1).pos
      prePipe(i).pos     := posOh(i).io.lsbPosXor
      prePipe(i).base    := postPipe(i-1).base
      prePipe(i).valid   := postPipe(i-1).valid
      prePipe(i).last    := postPipe(i-1).last
    }
    posOh(i).io.bypass := posOhCapt(i).valid
    prePipeVld(i) := prePipe(i).valid
  }

  val pipeQ = Reg(Vec(tLen, new PosBaseLastVld))

  for(i <- 0 until ffNumPolyEVal) {
    pipeQ(i) := prePipe((i*ffStepPosToNum)+ffStepPosToNum-1)
    postPipe((i*ffStepPosToNum)+ffStepPosToNum-1) := pipeQ(i)
  }
  for(i <- 0 until tLen) {
    if((i+1) % ffStepPosToNum != 0)
      postPipe(i) := prePipe(i)
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

//class Pipeline[D <: Data](data: D, ffNum: Int, ffStep: Int) extends Module{
//  val io = IO(new Bundle{
//    val prePipe = Input(Vec(ffNum, data))
//    val postPipe = Output(Vec(ffNum-1, data))
//  })
//
//  val pipeQ = Reg(Vec(tLen, new data))
//  for(i <- 0 until ffNum) {
//    pipeQ(i) := prePipe((i*ffStep)+ffStep-1)
//    postPipe((i*ffStep)+ffStep-1) := pipeQ(i)
//  }
//
//  for(i <- 0 until tLen) {
//    if((i+1) % ffStep != 0)
//      postPipe(i) := prePipe(i)
//  }
//}

//
// runMain Rs.GenTest
object GenTest extends App {
  //ChiselStage.emitSystemVerilogFile(new GfPolyEval(), Array())
  //ChiselStage.emitSystemVerilogFile(new RsChienGetBitPos(), Array())
  ChiselStage.emitSystemVerilogFile(new RsChienBitPosToNum(), Array())  
}

/////////////////////////////////////////
// Pipelining
/////////////////////////////////////////
