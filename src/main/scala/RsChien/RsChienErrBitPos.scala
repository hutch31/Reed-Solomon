package Rs

import chisel3._
import chisel3.util._
/////////////////////////////////////////
// RsChienErrBitPos 
/////////////////////////////////////////

class RsChienErrBitPos extends Module with GfParams {
  val io = IO(new Bundle {
    val errLocIf = Input(Valid(new vecFfsIf(tLen+1)))
    val bitPos = Output(new BitPosIf)
  })
  // Localparams
  val rootsNum = symbNum - 1
  val numOfCycles = math.ceil(rootsNum/chienRootsPerCycle.toDouble).toInt
  val chienNonValid = ((1 << (rootsNum % chienRootsPerCycle)) -1)

  val polyEval = for(i <- 0 until chienRootsPerCycle) yield Module(new GfPolyEvalHorner(tLen+1, chienHornerComboLen, chienHorner))
  //val polyEval = for(i <- 0 until chienRootsPerCycle) yield Module(new GfPolyEval(tLen+1))
  
  val roots = Wire(Valid(Vec(chienRootsPerCycle, UInt(symbWidth.W))))
  
  if(numOfCycles == 1) {
    for(i <- 0 until chienRootsPerCycle) {
      roots.bits := alphaToSymb(i.U)
    }
    roots.valid := io.errLocIf.valid
  } else {

    val cntrUpLimit = (numOfCycles-1)*chienRootsPerCycle
    val cntr = RegInit(UInt(log2Ceil(rootsNum).W), 0.U)

    when(io.errLocIf.valid === 1.U) {
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
    roots.valid := io.errLocIf.valid | (cntr =/= 0.U)
  }

  for(i <- 0 until chienRootsPerCycle) {
    polyEval(i).io.coefVec.bits := io.errLocIf.bits.vec
    polyEval(i).io.coefVec.valid := roots.valid
    polyEval(i).io.x := roots.bits(i)
    if(chienHorner)
      polyEval(i).io.coefFfs.get := io.errLocIf.bits.ffs
  }

  val errVal = VecInit(polyEval.map(_.io.evalValue.bits))
  // Capture EvalVal into register
  val bitPos = Reg(Vec(chienRootsPerCycle, Bool() ))
  bitPos := errVal.map(x => ~x.orR)

  // We can use any Valid here, so take (0)
  val bitPosVld = polyEval(0).io.evalValue.valid
  dontTouch(bitPosVld)
  val bitPosVldQ = RegNext(next=bitPosVld, init=false.B)

  val lastCycle = Wire(Bool())
  when(bitPosVld === 0.U & bitPosVldQ === 1.U){
    lastCycle := 1.U
    io.bitPos.last := 1.U
  }.otherwise{
    lastCycle := 0.U
    io.bitPos.last := 0.U
  }

  io.bitPos.valid := bitPosVldQ

  when(lastCycle & chienNonValid.U =/= 0.U) {
    // Nulify bits that is not valid.
    io.bitPos.pos := bitPos.asTypeOf(UInt(chienRootsPerCycle.W)) & chienNonValid.U
  }.otherwise{
    io.bitPos.pos := bitPos.asTypeOf(UInt(chienRootsPerCycle.W))
  }
  
}
