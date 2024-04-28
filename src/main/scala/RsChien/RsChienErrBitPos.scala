package Rs

import chisel3._
import chisel3.util._
/////////////////////////////////////////
// RsChienErrBitPos 
/////////////////////////////////////////

class RsChienErrBitPos extends Module with GfParams {
  val io = IO(new Bundle {
    val errLocatorIf = Input(Valid(new ErrLocatorBundle()))
    val bitPos = Output(new BitPosIf)
  })

  val polyEval = for(i <- 0 until chienRootsPerCycle) yield Module(new GfPolyEval())
  val base = Wire(Vec(chienRootsPerCycle, UInt(symbWidth.W)))
  val roots = Wire(Valid(Vec(chienRootsPerCycle, UInt(symbWidth.W))))
  
  for(i <- 0 until chienRootsPerCycle) {
    base(i) := i.U
  }

  // Localparams
  val rootsNum = symbNum - 1
  val numOfCycles = math.ceil(rootsNum/chienRootsPerCycle.toDouble).toInt
  val chienNonValid = ((1 << (rootsNum % chienRootsPerCycle)) -1)
  println("chienNonValid: " + chienNonValid)

  if(numOfCycles == 1) {
    for(i <- 0 until chienRootsPerCycle) {
      roots.bits := i.U
    }
    roots.valid := io.errLocatorIf.valid
  } else {

    val cntrUpLimit = (numOfCycles-1)*chienRootsPerCycle
    val cntr = RegInit(UInt(log2Ceil(rootsNum).W), 0.U)

    when(io.errLocatorIf.valid === 1.U) {
      cntr := chienRootsPerCycle.U
    }.elsewhen(cntr =/= 0.U) {
      when(cntr =/= (cntrUpLimit).U) {
        cntr := cntr + chienRootsPerCycle.U
      }.otherwise {
        cntr := 0.U
      }
    }

    for(i <- 0 until chienRootsPerCycle) {
      roots.bits(i) := alphaToSymb(cntr + i.U)
    }
    roots.valid := io.errLocatorIf.valid | (cntr =/= 0.U)
  }

  // Connect GfPolyEval
  for(i <- 0 until chienRootsPerCycle) {
    // Is it possible to use <> here to connect bundles
    polyEval(i).io.errLocatorIf.errLocator := io.errLocatorIf.bits.errLocator
    polyEval(i).io.errLocatorIf.errLocatorSel := io.errLocatorIf.bits.errLocatorSel
    polyEval(i).io.inSymb.bits := roots.bits(i)
    polyEval(i).io.inSymb.valid := roots.valid
  }

  val errVal = polyEval.map(_.io.evalValue.bits)
  // Capture EvalVal into register
  val bitPos = Reg(Vec(chienRootsPerCycle, Bool() ))
  bitPos := errVal.map(x => ~x.orR)

  // We can use any Valid here, so take (0)
  val bitPosVld = RegNext(next=polyEval(0).io.evalValue.valid, init=false.B)

  val lastCycle = Wire(Bool())
  when(polyEval(0).io.evalValue.valid === 0.U & bitPosVld === 1.U){
    lastCycle := 1.U
    io.bitPos.last := 1.U
  }.otherwise{
    lastCycle := 0.U
    io.bitPos.last := 0.U
  }

  io.bitPos.valid := bitPosVld

  when(lastCycle & chienNonValid.U =/= 0.U) {
    // Nulify bits that is not valid.
    io.bitPos.pos := bitPos.asTypeOf(UInt(chienRootsPerCycle.W)) & chienNonValid.U
  }.otherwise{
    io.bitPos.pos := bitPos.asTypeOf(UInt(chienRootsPerCycle.W))
  }
  
}
