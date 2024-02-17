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
    val prioEnvOut = Output(UInt(clog2(tLen).W))
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
    }
    else {
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

  /////////////////////////////////////////
  // Add registers
  /////////////////////////////////////////
  
  io.prioEnvOut := 0.U
  for(i <- (0 until tLen)) {
    when(io.errLocatorIf.errLocatorSel(i) === 1) {
      io.prioEnvOut := i.U
    }
  }

  io.evalValue.valid := prePipe(io.prioEnvOut).valid
  io.evalValue.bits := prePipe(io.prioEnvOut).xor
  

}

//class RsChien extends Module with GfParams {
//  val io = IO(new Bundle {
//    val errLocatorIf = Input(Valid(new ErrLocatorBundle())
//    val evalValue = Output(Valid(UInt(symbWidth.W)))
//  })
//
//  val polyEval = for(i <- 0 until chienRootsPerCycle) yield Module(new GfPolyEval())
//  val base = Wire(Vec(chienRootsPerCycle, symbWidth))
//  val roots = Wire(Vec(chienRootsPerCycle, symbWidth))
//
//  // Generates Chien roots
//  for(i <- 0 until chienRootsPerCycle) {
//    base := i.U
//  }
//
//  if(chienRootsPerCycle == 1) {
//    roots := base
//  } else {
//
//    val cntr = Reg(UInt(clog(chienCyclesNum.W)))
//    val offset = Reg(UInt(symbWidth.W))
//
//    when(errLocator.valid === 1) {
//      when(cntr !== (chienCyclesNum-1).U) {
//        cntr := cntr + 1.U
//        offset := offset + chienRootsPerCycle
//      } otherwise {
//        cntr := 0.U
//        offset := 0.U
//      }
//    }
//
//    roots := base + offset
//
//  }
//
//  // Connect GfPolyEval and roots
//  for(i <- 0 until chienRootsPerCycle) {
//    polyEval.io.ErrlocatorIf <> io.errlocatorif
//  }
//
//
//  
//
//
//}
//
// runMain Rs.GenTest
object GenTest extends App {
  ChiselStage.emitSystemVerilogFile(new GfPolyEval(), Array())
  //ChiselStage.emitSystemVerilogFile(new GfMult(), Array())
}
