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

class RsChien extends Module with GfParams {
  val io = IO(new Bundle {
    val errLocatorIf = Input(Valid(new ErrLocatorBundle()))
    val bitPos = Output(Valid(UInt(chienRootsPerCycle.W)))
    //val bitPosOut = Output(Valid(Vec(chienCyclesNum, UInt(chienRootsPerCycle.W))))
  })

  val polyEval = for(i <- 0 until chienRootsPerCycle) yield Module(new GfPolyEval())
  val base = Wire(Vec(chienRootsPerCycle, UInt(symbWidth.W)))
  val roots = Wire(Valid(Vec(chienRootsPerCycle, UInt(symbWidth.W))))
  val evalValue = Wire(Vec(chienRootsPerCycle, Valid(UInt(symbWidth.W))))
  //val bitPos = Wire(Valid(UInt(chienRootsPerCycle.W)))
  // Generates Chien roots
  for(i <- 0 until chienRootsPerCycle) {
    base(i) := i.U
  }

  if(chienRootsPerCycle == 1) {
    roots.bits := base
    roots.valid := io.errLocatorIf.valid
  } else {

    val cntr = Reg(UInt(log2Ceil(chienCyclesNum).W))
    val offset = Reg(UInt(symbWidth.W))

    when(io.errLocatorIf.valid === 1) {
      when(cntr =/= (chienCyclesNum-1).U) {
        cntr := cntr + 1.U
        offset := offset + chienRootsPerCycle
      } otherwise {
        cntr := 0.U
        offset := 0.U
      }
    }

    for(i <- 0 until chienRootsPerCycle) {
      roots.bits(i) := base(i) + offset
    }
    roots.valid := io.errLocatorIf.valid | (cntr =/= 0)

  }

  // Connect GfPolyEval
  for(i <- 0 until chienRootsPerCycle) {
    polyEval(i).io.errLocatorIf <> io.errLocatorIf.bits
    polyEval(i).io.inSymb.bits := roots.bits(i)
    polyEval(i).io.inSymb.valid := roots.valid
    polyEval(i).io.evalValue <> evalValue(i)
    when(evalValue(i).bits === 0.U) {
      io.bitPos.bits(i) := 0
    } otherwise {
      io.bitPos.bits(i) := 1
    }
  }


}


//
// runMain Rs.GenTest
object GenTest extends App {
  ChiselStage.emitSystemVerilogFile(new GfPolyEval(), Array())
  ChiselStage.emitSystemVerilogFile(new RsChien(), Array())
}
