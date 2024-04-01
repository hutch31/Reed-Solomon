package Rs

import chisel3._
import chisel3.util._

class ErrEval extends Module with GfParams {
  val io = IO(new Bundle {
    val errataLocIf = Input(Valid(new vecFfsIf(tLen+1)))
    val syndrome = Input(Vec(redundancy, UInt(symbWidth.W)))
    val errEvalIf = Output(Valid(Vec(tLen+1, UInt(symbWidth.W))))
  })

  val diagXorAll = Module(new DiagonalXorAll(tLen+1, redundancy, symbWidth))
  val syndromeRev = Wire(Vec(redundancy, UInt(symbWidth.W)))
  val syndXErrataLocArray = Wire(Vec(tLen+1, Vec(redundancy, UInt(symbWidth.W))))
  
  for(i <- 0 until redundancy) {
    syndromeRev(i) := io.syndrome(redundancy-1-i)
  }

  // Poly Mult: multiply syndrome and errata locator

  for(locIndx <- 0 until tLen+1) { // tLen = 8 / redundancy = 16
    for(syndIndx <- 0 until redundancy) {
      syndXErrataLocArray(locIndx)(syndIndx) := gfMult(io.errataLocIf.bits.vec(locIndx), syndromeRev(syndIndx))
    }
  }
  
  diagXorAll.io.recMatrix := syndXErrataLocArray

  // Pipeline
  val syndXErrataLoc = RegNext(diagXorAll.io.xorVect)
  val syndXErrataLocVld = RegNext(next=io.errataLocIf.valid, init=false.B)
  val posFfsQ = RegNext(io.errataLocIf.bits.ffs)

  // Poly Divide
  // Divisor could vary from 1'b1 up to { 1'b1, {T_LEN-1{1'b0}} }

  val errorEvalArray = Wire(Vec(tLen, (Vec(tLen, UInt(symbWidth.W)))))

  for(wordIndx <- 0 until tLen) {
    for(symbIndx <- 0 until tLen) {
      if(symbIndx > wordIndx)
        errorEvalArray(wordIndx)(symbIndx) := 0.U
      else
        errorEvalArray(wordIndx)(symbIndx) := syndXErrataLoc(wordIndx-symbIndx)
    }
  }

  val errEval = Mux1H(posFfsQ, errorEvalArray)
  val errEvalExp = Wire(Vec(tLen+1, UInt(symbWidth.W)))
  val errEvalExpQ = Reg(Vec(tLen+1, UInt(symbWidth.W)))

  // Expand errEval vec
  errEvalExp := errEval ++ 0.U.asTypeOf(Vec(1, UInt(symbWidth.W)))

  when(syndXErrataLocVld) {
    errEvalExpQ := errEvalExp
  }

  io.errEvalIf.valid := RegNext(next=syndXErrataLocVld, init=false.B)
  io.errEvalIf.bits := errEvalExpQ

}
