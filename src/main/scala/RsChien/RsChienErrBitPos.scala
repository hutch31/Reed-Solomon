package Rs

import chisel3._
import chisel3.util._
/////////////////////////////////////////
// RsChienErrBitPos 
/////////////////////////////////////////

class RsChienErrBitPos(c: Config) extends Module{
  val io = IO(new Bundle {
    val errLocIf = Input(Valid(new vecFfsIf(c.T_LEN+1, c.SYMB_WIDTH)))
    val bitPos = Output(new BitPosIf(c.chienRootsPerCycle))
  })
  // Localparams
  val rootsNum = c.SYMB_NUM - 1
  val chienNonValid = ((BigInt(1) << (rootsNum % c.chienRootsPerCycle)) -1)
  println(s"rootsNum = $rootsNum $chienNonValid")

  // TODO: Create the PolyEval depends on the input param
  //val polyEval = for(i <- 0 until c.chienRootsPerCycle) yield Module(new GfPolyEvalHorner(c.T_LEN+1, chienHornerComboLen, chienHorner))
  val polyEval = for(i <- 0 until c.chienRootsPerCycle) yield Module(new GfPolyEval(c, c.T_LEN+1))

  ////////////////////////////////////
  // Generate roots to substitute into the equation
  ////////////////////////////////////
  
  val roots = Wire(Valid(Vec(c.chienRootsPerCycle, UInt(c.SYMB_WIDTH.W))))
  
  if(c.chienErrBitPosLatency == 1) {
    for(i <- 0 until c.chienRootsPerCycle) {
      roots.bits := c.alphaToSymb(i.U)
    }
    roots.valid := io.errLocIf.valid
  } else {

    val cntrUpLimit = c.chienErrBitPosLatency*c.chienRootsPerCycle
    val cntr = RegInit(UInt(log2Ceil(rootsNum).W), 0.U)

    when(io.errLocIf.valid === 1.U) {
      cntr := c.chienRootsPerCycle.U
    }.elsewhen(cntr =/= 0.U) {
      when(cntr =/= (cntrUpLimit).U) {
        cntr := cntr + c.chienRootsPerCycle.U
      }.otherwise {
        cntr := 0.U
      }
    }

    for(i <- 0 until c.chienRootsPerCycle) {
      roots.bits(i) := c.alphaToSymb(cntr + i.U)
    }
    roots.valid := io.errLocIf.valid | (cntr =/= 0.U)
  }

  for(i <- 0 until c.chienRootsPerCycle) {
    polyEval(i).io.coefVec.bits := io.errLocIf.bits.vec
    polyEval(i).io.coefVec.valid := roots.valid
    polyEval(i).io.x := roots.bits(i)
    if(c.chienHorner)
      polyEval(i).io.coefFfs.get := io.errLocIf.bits.ffs
  }

  val errVal = VecInit(polyEval.map(_.io.evalValue.bits))
  // Capture EvalVal into register
  val bitPos = Reg(Vec(c.chienRootsPerCycle, Bool() ))
  dontTouch(bitPos)

  bitPos := errVal.map(x => ~x.orR)

  // We can use any Valid here, so take (0)
  val bitPosVld = polyEval(0).io.evalValue.valid
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
    io.bitPos.pos := bitPos.asTypeOf(UInt(c.chienRootsPerCycle.W)) & chienNonValid.U
  }.otherwise{
    io.bitPos.pos := bitPos.asTypeOf(UInt(c.chienRootsPerCycle.W))
  }
  
}
