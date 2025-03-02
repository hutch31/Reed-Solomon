package Rs

import chisel3._
import chisel3.util._
/////////////////////////////////////////
// RsChienErrBitPos 
/////////////////////////////////////////

class RsChienErrBitPos(c: Config) extends Module{
  val io = IO(new Bundle {
    val errLocIf = Input(Valid(new vecFfsIf(c.T_LEN+1, c.SYMB_WIDTH)))
    val bitPos = Output(new BitPosIf(c.chienErrBitPosTermsPerCycle))
  })
  // Localparams
  //val rootsNum = c.SYMB_NUM - 1
  val rootsNum = c.N_LEN
  val chienNonValid = ((BigInt(1) << (rootsNum % c.chienErrBitPosTermsPerCycle)) -1)
  val polyEval = for(i <- 0 until c.chienErrBitPosTermsPerCycle) yield Module(new GfPolyEval(c, c.T_LEN+1))

  ////////////////////////////////////
  // Generate roots to substitute into the equation
  ////////////////////////////////////
  
  val roots = Wire(Valid(Vec(c.chienErrBitPosTermsPerCycle, UInt(c.SYMB_WIDTH.W))))
  val rootsLast = Wire(Bool())

  if(c.chienErrBitPosTermsPerCycle >= c.N_LEN) {
    for(i <- 0 until c.chienErrBitPosTermsPerCycle) {
      roots.bits(i) := c.alphaToSymb(i.U)
    }
    roots.valid := io.errLocIf.valid
    rootsLast := io.errLocIf.valid
  } else {

    val cntrUpLimit = (c.chienErrBitPosShiftLatency-1)*c.chienErrBitPosTermsPerCycle
    val cntr = RegInit(UInt(log2Ceil(rootsNum).W), 0.U)

    rootsLast := cntr === (cntrUpLimit).U
    
    when(io.errLocIf.valid === 1.U) {
      cntr := c.chienErrBitPosTermsPerCycle.U
    }.elsewhen(cntr =/= 0.U) {
      when(cntr =/= (cntrUpLimit).U) {
        cntr := cntr + c.chienErrBitPosTermsPerCycle.U
      }.otherwise {
        cntr := 0.U
      }
    }

    for(i <- 0 until c.chienErrBitPosTermsPerCycle) {
      roots.bits(i) := c.alphaToSymb(cntr + i.U)
    }
    roots.valid := io.errLocIf.valid | (cntr =/= 0.U)
  }

  for(i <- 0 until c.chienErrBitPosTermsPerCycle) {
    polyEval(i).io.coefVec.bits := io.errLocIf.bits.vec
    polyEval(i).io.coefVec.valid := roots.valid
    polyEval(i).io.x := roots.bits(i)
    if(c.chienHorner)
      polyEval(i).io.coefFfs.get := io.errLocIf.bits.ffs
  }

  val errVal = VecInit(polyEval.map(_.io.evalValue.bits))

  ///////////////////////////////////
  // Capture EvalVal into register
  ///////////////////////////////////

  val bitPos = Reg(Vec(c.chienErrBitPosTermsPerCycle, Bool() ))
  dontTouch(bitPos)

  bitPos := errVal.map(x => ~x.orR)

  // We can use any Valid here, so take (0)
  val bitPosVld = polyEval(0).io.evalValue.valid

  val bitPosVldQ = RegNext(next=bitPosVld, init=false.B)
  val bitPosLastQ = RegNext(next=rootsLast, init=false.B)

  io.bitPos.last := bitPosLastQ

  io.bitPos.valid := bitPosVldQ

  when(bitPosLastQ & chienNonValid.U =/= 0.U) {
    // Nulify bits that is not valid.
    io.bitPos.pos := bitPos.asTypeOf(UInt(c.chienErrBitPosTermsPerCycle.W)) & chienNonValid.U
  }.otherwise{
    io.bitPos.pos := bitPos.asTypeOf(UInt(c.chienErrBitPosTermsPerCycle.W))
  }
  
}
