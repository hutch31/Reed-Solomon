package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class CapturePositions(c:Config) extends Module {
  val io = IO(new Bundle {
    val bitPos = Input(new BitPosIf(c.chienRootsPerCycle))
    val errPosIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))    
  })

  ////////////////////////////////
  // Extract position from incomming 
  // bit vector where bits are asserted
  ////////////////////////////////

  val extrPos = Module(new ExtractPositions(c.chienRootsPerCycle, c.T_LEN, 4))
  val numStages = extrPos.numStages

  extrPos.io.in.bits := io.bitPos.pos
  extrPos.io.in.valid := io.bitPos.valid

  // Shift last
  val lastQ = Reg(Vec(numStages, Bool()))

  lastQ(0) := io.bitPos.last
  for(i <- 1 until numStages) {
    lastQ(i) := lastQ(i-1)
  }

  ////////////////////////////////
  // insertVec is used to capture and shift
  // valid positions into shift register of
  // length T_LEN.
  //
  // outShift is used to remove bubbles in
  // the shift registers since number of
  // errors could be less than T_LEN.
  ////////////////////////////////

  val insertVec = Module(new InsertVec(new PositionsVld,c.T_LEN, c.T_LEN))
  val baseVec = Reg(Vec(c.T_LEN, new PositionsVld))
  val vecIn   = Wire(Vec(c.T_LEN, new PositionsVld))
  val baseCntr = RegInit(UInt(log2Ceil(c.SYMB_NUM).W), 0.U)

  // Final shift
  val outShift = Module(new BarrelShifter(new PositionsVld, c.T_LEN))
  val outputVec = Reg(Vec(c.T_LEN, new PositionsVld))

  class PositionsVld extends Bundle {
    val positions = UInt(c.SYMB_WIDTH.W)
    val vld = Bool()
  }

  insertVec.io.vecIn := vecIn
  insertVec.io.baseVec := baseVec
  insertVec.io.shiftVal := extrPos.io.outTkeep.map(_.asUInt).reduce(_ +& _)

  outShift.io.vecIn := insertVec.io.vecOut
  outShift.io.shiftVal := insertVec.io.vecOut.map(bundle => (!bundle.vld).asUInt).reduce(_ +& _)

  // need to add baseCntr to get a proper position:
  when(extrPos.io.outValid) {
    when(lastQ(numStages-1)) {
      baseCntr := 0.U
      baseVec.foreach(x => x := 0.U.asTypeOf(new PositionsVld))
      outputVec := outShift.io.vecOut
      //outputVec := outShift.io.vecOut
    }.otherwise{
      baseCntr := baseCntr + c.chienRootsPerCycle.U
      baseVec := insertVec.io.vecOut
    }
  }

  for(i <- 0 until c.T_LEN) {
    vecIn(i).positions := extrPos.io.positions(i) + baseCntr
    vecIn(i).vld := extrPos.io.outTkeep(i)
    io.errPosIf.bits.vec(i) := outputVec(i).positions
  }

  //io.errPosIf.bits.ffs := Reverse((VecInit(outputVec.map(_.vld))).asUInt)
  //io.errPosIf.bits.vec := insertVec.io.vecOut
  
  io.errPosIf.valid := lastQ(numStages-1)
  io.errPosIf.bits.ffs := (VecInit(insertVec.io.vecOut.map(_.vld))).asUInt
  io.errPosIf.bits.vec := (VecInit(insertVec.io.vecOut.map(_.positions)))
}

object GenCapturePositions extends App {

  val projectRoot = System.getProperty("project.root")

  ConfigParser.parse(args) match {
    case Some(config) =>
      JsonHandler.writeToFile(config, "rs.json")
      // Get 
      val rsCfg = RSDecoderConfigs.getConfig(config.N_LEN, config.K_LEN)
      val c = Config(config, rsCfg)
      ChiselStage.emitSystemVerilogFile(new CapturePositions(c), Array())
    case None =>
      sys.exit(1)      
  }  
}
