package Rs

import chisel3._
import chisel3.util._

class ErrEval extends Module with GfParams {
  val io = IO(new Bundle {
    val errataLocIf = Input(Valid(new vecFfsIf(tLen+1)))
    val syndrome = Input(Vec(redundancy, UInt(symbWidth.W)))
    val errEvalIf = Output(Valid(new vecFfsIf(tLen+1)))
  })

  val syndRev = Wire(Vec(redundancy, UInt(symbWidth.W)))
  
  for(i <- 0 until redundancy) {
    syndRev(i) := io.syndrome(redundancy-1-i)
  }

  ///////////////////////////////////
  // Shift errataLoc
  ///////////////////////////////////

  val shiftVec = Module(new ShiftBundleMod(UInt(symbWidth.W), tLen+1, numOfSymbEe))
  shiftVec.io.vecIn.bits  := io.errataLocIf.bits.vec
  shiftVec.io.vecIn.valid := io.errataLocIf.valid

  // Connect shift output to combo stage(s)
  val stage = for(i <- 0 until numOfSymbEe) yield Module(new ErrEvalStage)
  val stageOut = Wire(Vec(numOfSymbEe, Vec(redundancy, UInt(symbWidth.W))))

  for(i <- 0 until numOfSymbEe) {
    stage(i).io.syndRev := syndRev
    stage(i).io.errataLocSymb := shiftVec.io.vecOut.bits(i)
    stageOut(i) := stage(i).io.syndXErrataLoc
  }

  ///////////////////////////////////
  // Accum matrix
  ///////////////////////////////////

  val numOfCycles = math.ceil((tLen+1)/numOfSymbEe.toDouble).toInt
  val accumMat = Module(new AccumMat(symbWidth, redundancy, numOfSymbEe, numOfCycles, tLen+1))
  accumMat.io.vecIn := stageOut
  val accumVld = RegNext(shiftVec.io.lastOut)

  ///////////////////////////////////
  // Calc Xor
  ///////////////////////////////////

  val diagXorAll = Module(new DiagonalXorAll(tLen+1, redundancy, symbWidth))

  diagXorAll.io.recMatrix := accumMat.io.matOut

  // Pipeline
  val syndXErrataLoc = RegNext(diagXorAll.io.xorVect)
  val syndXErrataLocVld = RegNext(next=accumVld, init=false.B)
  
  ///////////////////////////////////
  // Poly Divide
  // Divisor could vary from 1'b1 up to { 1'b1, {T_LEN-1{1'b0}} }
  ///////////////////////////////////

  val errorEvalArray = Wire(Vec(tLen, (Vec(tLen, UInt(symbWidth.W)))))

  for(wordIndx <- 0 until tLen) {
    for(symbIndx <- 0 until tLen) {
      if(symbIndx > wordIndx)
        errorEvalArray(wordIndx)(symbIndx) := 0.U
      else
        errorEvalArray(wordIndx)(symbIndx) := syndXErrataLoc(wordIndx-symbIndx)
    }
  }

  val errEval = Mux1H(io.errataLocIf.bits.ffs, errorEvalArray)
  val errEvalExp = Wire(Vec(tLen+1, UInt(symbWidth.W)))
  val errEvalExpQ = Reg(Vec(tLen+1, UInt(symbWidth.W)))

  val errEvalFfs = RegInit(UInt((tLen+1).W), 0.U)

  // Expand errEval vec
  errEvalExp := errEval ++ 0.U.asTypeOf(Vec(1, UInt(symbWidth.W)))

  // Capture vec and ffs
  when(syndXErrataLocVld) {
    errEvalFfs := io.errataLocIf.bits.ffs
    errEvalExpQ := errEvalExp
  }

  io.errEvalIf.valid := RegNext(next=syndXErrataLocVld, init=false.B)
  io.errEvalIf.bits.vec := errEvalExpQ
  io.errEvalIf.bits.ffs := errEvalFfs

}

class ErrEvalStage extends Module with GfParams {
  val io = IO(new Bundle {
    val errataLocSymb = Input(UInt(symbWidth.W))
    val syndRev = Input(Vec(redundancy, UInt(symbWidth.W)))
    val syndXErrataLoc = Output(Vec(redundancy, UInt(symbWidth.W)))
  })

  for(syndIndx <- 0 until redundancy) {
    io.syndXErrataLoc(syndIndx) := gfMult(io.errataLocSymb, io.syndRev(syndIndx))
  }

}
