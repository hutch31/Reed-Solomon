package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class ExtractPositions(val InWidth: Int, val PosNum: Int, val pipelineInterval: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Valid(UInt(InWidth.W)))
    val positions = Output(Vec(PosNum, UInt(log2Ceil(InWidth).W)))
    val outTkeep = Output(Vec(PosNum, Bool()))
    val outValid = Output(Bool())
  })

  // Number of pipeline stages required
  val numStages = (InWidth + pipelineInterval - 1) / pipelineInterval

  // Convert input to a vector of Bools
  val inVec = io.in.bits.asBools

  val prefSumOut = Wire(Vec(InWidth, UInt(log2Ceil(InWidth).W)))
  val prefSumVldOut = Wire(Bool())

  val inOut = Wire(UInt(InWidth.W))
  val inQ = Reg(Vec(numStages, (UInt(InWidth.W))))
  val inVecQ = Reg(Vec(numStages, (Vec(InWidth, Bool()))))

  val prefSumQ = Reg(Vec(numStages, Vec(InWidth, UInt(log2Ceil(InWidth).W))))
  val vldQ = Reg(Vec(numStages, Bool()))
  
  // Compute prefix sum with pipeline registers
  for (stage <- 0 until numStages+1) {

    val prefSumInit = Wire(Vec(InWidth, UInt(log2Ceil(InWidth).W)))
    val prefSum = Wire(Vec(InWidth, UInt(log2Ceil(InWidth).W)))

    if(stage == 0) {
      prefSumInit := VecInit(inVec.map(b => Mux(b, 1.U, 0.U)))
    } else {
      prefSumInit := prefSumQ(stage-1)
    }

    val startIdx = stage * pipelineInterval
    val endIdx = stage * pipelineInterval + pipelineInterval

    prefSum(0) := prefSumInit(0)

    for(pos <- 1 until InWidth) {
      if(pos < startIdx)
        prefSum(pos) := prefSumInit(pos)
      else if(pos < endIdx)
        prefSum(pos) := prefSumInit(pos) + prefSum(pos-1)
      else
        prefSum(pos) := prefSumInit(pos)
    }

    if(stage == numStages) {
      prefSumOut := prefSum.map(b => b - 1.U)
      prefSumVldOut := vldQ(stage-1)
      inOut := inQ(stage-1)
    }
    else {
      prefSumQ(stage) := prefSum
      if(stage == 0) {
        vldQ(stage) := io.in.valid
        inQ(stage) := io.in.bits
      }
      else {
        vldQ(stage) := vldQ(stage-1)
        inQ(stage) := inQ(stage-1)
      }
    }
  }

  // Initialize outputs
  val positionsReg = Reg(Vec(PosNum, UInt(log2Ceil(InWidth).W)))
  val outTkeepReg = Reg(Vec(PosNum, Bool()))
  val outputOutValid = RegInit(false.B)

  // Default assignments
  for (i <- 0 until PosNum) {
    positionsReg(i) := 0.U
    outTkeepReg(i) := false.B
  }

  // Assign positions
  for (i <- 0 until InWidth) {
    when(inOut.asBools(i) && prefSumOut(i) < PosNum.U) {
      val index = prefSumOut(i)(log2Ceil(PosNum)-1, 0) // Truncate prefSumOut(i) to the correct width
      positionsReg(index) := i.U
      outTkeepReg(index) := true.B
    }
  }

  // Connect outputs
  io.positions := positionsReg
  io.outTkeep := outTkeepReg
  io.outValid := RegNext(prefSumVldOut, init=false.B )
  //io.outValid := io.in.valid
}

object GenExtractPositions extends App {
  ChiselStage.emitSystemVerilogFile(new ExtractPositions(8, 4, 4), Array())
}
