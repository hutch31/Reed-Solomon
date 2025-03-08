/*
 * ----------------------------------------------------------------------
 *  Copyright (c) 2024 Egor Smirnov
 *
 *  Licensed under terms of the MIT license
 *  See https://github.com/egorman44/Reed-Solomon/blob/main/LICENSE
 *    for license terms
 * ----------------------------------------------------------------------
 */

package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class CapturePositions(c:Config) extends Module {
  val io = IO(new Bundle {
    val bitPos = Input(new BitPosIf(c.chienErrBitPosTermsPerCycle))
    val errPosIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))    
  })

  class PositionsVld extends Bundle {
    val positions = UInt(c.SYMB_WIDTH.W)
    val vld = Bool()
  }

  val zeroUnit = 0.U.asTypeOf(new PositionsVld)

  ////////////////////////////////
  // Extract position from incomming 
  // bit vector where bits are asserted
  ////////////////////////////////
  val PosNum = if(c.T_LEN > c.chienErrBitPosTermsPerCycle) c.chienErrBitPosTermsPerCycle else c.T_LEN
  val extrPos = Module(new ExtractPositions(c.chienErrBitPosTermsPerCycle, PosNum, c.chienBitPosPipeIntrvl))

  val numStages = extrPos.numStages + 1

  extrPos.io.in.bits := io.bitPos.pos
  extrPos.io.in.valid := io.bitPos.valid

  // Shift last
  val lastQ = RegInit(VecInit(Seq.fill(numStages)(false.B)))

  lastQ(0) := io.bitPos.last
  for(i <- 1 until numStages) {
    lastQ(i) := lastQ(i-1)
  }

  ////////////////////////////////
  // insertVec is used to capture and shift
  // valid positions into shift register of
  // length T_LEN.
  ////////////////////////////////

  // Output width is T_LEN+1 to capture the error condition
  val insertVec = Module(new InsertVec(new PositionsVld,c.T_LEN, c.T_LEN))

  val vecIn   = Wire(Vec(c.T_LEN, new PositionsVld))
  val baseCntr = RegInit(UInt(log2Ceil(c.SYMB_NUM).W), 0.U)
  //val baseVec = Reg(Vec(c.T_LEN, new PositionsVld))
  val baseVec = RegInit(VecInit(Seq.fill(c.T_LEN)(zeroUnit)))
  val outReg  = RegInit(VecInit(Seq.fill(c.T_LEN)(zeroUnit)))

  //The shift value is defined by the valid numbers in the vector.
  insertVec.io.shiftVal := baseVec.map(_.vld.asUInt).reduce(_ +& _)
  insertVec.io.vecIn := vecIn
  insertVec.io.baseVec := baseVec
  
  // need to add baseCntr to get a proper position:
  when(extrPos.io.outValid) {
    when(lastQ(numStages-1)) {
      baseCntr := 0.U
      baseVec.foreach(x => x := 0.U.asTypeOf(new PositionsVld))
    }.otherwise{
      baseCntr := baseCntr + c.chienErrBitPosTermsPerCycle.U
      // Capture only valid output
      for(i <- 0 until c.T_LEN) {
        baseVec(i).vld := insertVec.io.vecOut(i).vld
        when(insertVec.io.vecOut(i).vld) {
          baseVec(i).positions := insertVec.io.vecOut(i).positions
        }
      }
    }
  }

  // If T_LEN > chienErrBitPosTermsPerCycle,
  // then we need to padd vecIn
  for(i <- 0 until c.T_LEN) {    
    if(i >= c.chienErrBitPosTermsPerCycle) {
      vecIn(i) := zeroUnit
    }
    else {
      vecIn(i).positions := extrPos.io.positions(i) + baseCntr
      vecIn(i).vld := extrPos.io.outTkeep(i)
    }
  }

  when(lastQ(numStages-1)){
    outReg := insertVec.io.vecOut
  }

  io.errPosIf.bits.ffs := (VecInit(outReg.map(_.vld))).asUInt
  io.errPosIf.bits.vec := (VecInit(outReg.map(x => Mux(x.vld, c.N_LEN.U - 1.U - x.positions, 0.U))))
  io.errPosIf.valid := RegNext(lastQ(numStages-1), init = false.B)

}

object GenCapturePositions extends App {

  val projectRoot = System.getProperty("project.root")

  ConfigParser.parse(args) match {
    case Some(config) =>
      JsonHandler.writeToFile(config, "rs.json")
      // Get 
      val c = Config(config)
      ChiselStage.emitSystemVerilogFile(new CapturePositions(c), Array())
    case None =>
      sys.exit(1)      
  }  
}
