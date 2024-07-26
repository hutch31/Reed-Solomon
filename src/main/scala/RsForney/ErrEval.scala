package Rs

import chisel3._
import chisel3.util._

class ErrEval(c: Config) extends Module{
  val io = IO(new Bundle {
    val errataLocIf = Input(Valid(new vecFfsIf(c.T_LEN+1, c.SYMB_WIDTH)))
    val syndIf = Input(Valid(Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W))))
    val errEvalIf = Output(Valid(new vecFfsIf(c.T_LEN+1, c.SYMB_WIDTH)))
  })

  val syndRev = Wire(Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W)))

  syndRev := io.syndIf.bits
  // TODO: check reverse Check the 
  //for(i <- 0 until c.REDUNDANCY) {
  //  syndRev(i) := io.syndIf.bits(c.REDUNDANCY-1-i)
  //}

  ///////////////////////////////////
  // Shift errataLoc
  ///////////////////////////////////

  val shiftVec = Module(new ShiftBundleMod(UInt(c.SYMB_WIDTH.W), c.T_LEN+1, c.numOfSymbEe))
  shiftVec.io.vecIn.bits  := io.errataLocIf.bits.vec
  shiftVec.io.vecIn.valid := io.errataLocIf.valid

  // Connect shift output to combo stage(s)
  val stage = for(i <- 0 until c.numOfSymbEe) yield Module(new ErrEvalStage(c))
  val stageOut = Wire(Vec(c.numOfSymbEe, Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W))))

  for(i <- 0 until c.numOfSymbEe) {
    stage(i).io.syndRev := syndRev
    stage(i).io.errataLocSymb := shiftVec.io.vecOut.bits(i)
    stageOut(i) := stage(i).io.syndXErrataLoc
  }

  ///////////////////////////////////
  // Accum matrix
  ///////////////////////////////////

  val numOfCycles = math.ceil((c.T_LEN+1)/c.numOfSymbEe.toDouble).toInt
  val accumMat = Module(new AccumMat(c.SYMB_WIDTH, c.REDUNDANCY, c.numOfSymbEe, numOfCycles, c.T_LEN+1))
  accumMat.io.vecIn := stageOut
  val accumVld = RegNext(shiftVec.io.lastOut)

  ///////////////////////////////////
  // Calc Xor
  ///////////////////////////////////

  val diagXorAll = Module(new DiagonalXorAll(c.T_LEN+1, c.REDUNDANCY, c.SYMB_WIDTH))

  diagXorAll.io.recMatrix := accumMat.io.matOut

  // Pipeline
  val syndXErrataLoc = RegNext(diagXorAll.io.xorVect)
  val syndXErrataLocVld = RegNext(next=accumVld, init=false.B)
  
  ///////////////////////////////////
  // Poly Divide
  // Divisor could vary from 1'b1 up to { 1'b1, {T_LEN-1{1'b0}} }
  ///////////////////////////////////

  val errorEvalArray = Wire(Vec(c.T_LEN, (Vec(c.T_LEN, UInt(c.SYMB_WIDTH.W)))))

  for(wordIndx <- 0 until c.T_LEN) {
    for(symbIndx <- 0 until c.T_LEN) {
      if(symbIndx > wordIndx)
        errorEvalArray(wordIndx)(symbIndx) := 0.U
      else
        errorEvalArray(wordIndx)(symbIndx) := syndXErrataLoc(wordIndx-symbIndx)
    }
  }

  val errEval = Mux1H(io.errataLocIf.bits.ffs, errorEvalArray)
  val errEvalExp = Wire(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))
  val errEvalExpQ = Reg(Vec(c.T_LEN+1, UInt(c.SYMB_WIDTH.W)))

  val errEvalFfs = RegInit(UInt((c.T_LEN+1).W), 0.U)

  // Expand errEval vec
  errEvalExp := errEval ++ 0.U.asTypeOf(Vec(1, UInt(c.SYMB_WIDTH.W)))

  // Capture vec and ffs
  when(syndXErrataLocVld) {
    errEvalFfs := io.errataLocIf.bits.ffs
    errEvalExpQ := errEvalExp
  }

  io.errEvalIf.valid := RegNext(next=syndXErrataLocVld, init=false.B)
  io.errEvalIf.bits.vec := errEvalExpQ
  io.errEvalIf.bits.ffs := errEvalFfs

}

class ErrEvalStage(c:Config) extends Module{
  val io = IO(new Bundle {
    val errataLocSymb = Input(UInt(c.SYMB_WIDTH.W))
    val syndRev = Input(Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W)))
    val syndXErrataLoc = Output(Vec(c.REDUNDANCY, UInt(c.SYMB_WIDTH.W)))
  })

  for(syndIndx <- 0 until c.REDUNDANCY) {
    io.syndXErrataLoc(syndIndx) := c.gfMult(io.errataLocSymb, io.syndRev(syndIndx))
  }

}
