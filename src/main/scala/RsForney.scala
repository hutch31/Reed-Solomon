package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

class ErrataLocatorPar extends Module with GfParams {
  val io = IO(new Bundle {
    val posArray = Input(new NumPosIf)
    val errataLoc = Output(Valid(Vec(tLen+1, UInt(symbWidth.W))))
  })

  val stage = for(i <- 0 until numOfErrataLocStages) yield Module(new ErrataLocatorStage())
  val ffs = Module(new FindFirstSet(symbWidth))
  ffs.io.in := io.posArray.sel

  val errPosShift    = Reg(Vec(tLen, UInt(symbWidth.W)))
  // Slice valid bit that goes into comb stage(s)

  val errataLoc = Reg(Vec(tLen+1, UInt(symbWidth.W)))
  val stageOut = Wire(Vec(numOfErrataLocStages, (Vec(tLen+1, UInt(symbWidth.W)))))

  val errPosVldShift = RegInit(UInt(tLen.W), 0.U)
  val errPosVldStage = errPosVldShift(numOfErrataLocStages - 1, 0)
  val errataLocVld = RegNext(next=errPosVldStage.orR, init=false.B)

  // Put data into the shift register

  when (io.posArray.valid) {
    errPosShift := io.posArray.pos
    errPosVldShift := ffs.io.out
    for(i <- 0 until tLen+1) {
      if(i == 0)
        errataLoc(i) := 1
      else
        errataLoc(i) := 0
    }
  }.otherwise {
    for(i <- 0 until tLen-numOfErrataLocStages) {
      errPosShift(i) := errPosShift(i+numOfErrataLocStages)
    }
    errPosVldShift := errPosVldShift >> numOfErrataLocStages
    when(errPosVldStage === 0) {
      errataLoc := stage(numOfErrataLocStages-1).io.errataLoc
    }.otherwise {
      if(numOfErrataLocStages == 1)
        errataLoc := stage(numOfErrataLocStages-1).io.errataLoc
      else
        errataLoc := Mux1H(errPosVldStage, stageOut)
    }
  }

  stage(0).io.errataLocPrev := errataLoc
  stage(0).io.errPos := errPosShift(0)
  stageOut(0) := stage(0).io.errataLoc

  if(numOfErrataLocStages > 1) {
    for(i <- 1 until numOfErrataLocStages) {
      stage(i).io.errataLocPrev := stage(i-1).io.errataLoc
      stage(i).io.errPos := errPosShift(i)
      stageOut(i) := stage(i).io.errataLoc
    }
  }

  io.errataLoc.valid := errataLocVld
  io.errataLoc.bits := errataLoc

}

class ErrorEvaluator extends Module with GfParams {
  val io = IO(new Bundle {
    val errataLoc = Input(Valid(Vec(tLen+1, UInt(symbWidth.W))))
    val syndrome = Input(Vec(redundancy, UInt(symbWidth.W)))
    val syndXErrataLoc = Output(Valid(Vec(redundancy+tLen, UInt(symbWidth.W))))
  })

  val diagXorAll = Module(new DiagonalXorAll(tLen+1, redundancy, symbWidth))
  val syndromeRev = Wire(Vec(redundancy, UInt(symbWidth.W)))
  val syndXErrataLocMatrix = Wire(Vec(tLen+1, Vec(redundancy, UInt(symbWidth.W))))
  
  for(i <- 0 until redundancy) {
    syndromeRev(i) := io.syndrome(redundancy-1-i)
  }

  for(locIndx <- 0 until tLen+1) { // tLen = 8 / redundancy = 16
    for(syndIndx <- 0 until redundancy) {
      syndXErrataLocMatrix(locIndx)(syndIndx) := gfMult(io.errataLoc.bits(locIndx), syndromeRev(syndIndx))
    }
  }

  // Poly Mult
  diagXorAll.io.recMatrix := syndXErrataLocMatrix

  val syndXErrataLoc = RegNext(diagXorAll.io.xorVect)
  val syndXErrataLocVld = RegNext(next=io.errataLoc.valid, init=false.B)

  io.syndXErrataLoc.bits := syndXErrataLoc
  io.syndXErrataLoc.valid := syndXErrataLocVld

  // Poly Divide
  // Divisor could vary from 1'b1 up to { 1'b1, {T_LEN-1{1'b0}} }
  // logic [T_LEN:0][SYMB_WIDTH-1:0] dividend_arr [T_LEN-1:0];
  //val dividendArray = Wire(Vec(tLen, (Vec(tLen, UInt(symbWidth.W)))))


}

class RsForney extends Module with GfParams {
  val io = IO(new Bundle {
    val posArray = Input(new NumPosIf)
    val syndrome = Input(Vec(redundancy, UInt(symbWidth.W)))
    val syndXErrataLoc = Output(Valid(Vec(redundancy+tLen, UInt(symbWidth.W))))
  })

  val errataLocator= Module(new ErrataLocatorPar)
  val errorEvaluator = Module(new ErrorEvaluator)

  errataLocator.io.posArray := io.posArray

  errorEvaluator.io.errataLoc := errataLocator.io.errataLoc
  errorEvaluator.io.syndrome := io.syndrome
  io.syndXErrataLoc := errorEvaluator.io.syndXErrataLoc

}


//class ErrataLocator extends Module with GfParams {
//  val io = IO(new Bundle {
//    val posArray = Input(new NumPosIf)
//    val errataLoc = Output(Valid(Vec(tLen+1, UInt(symbWidth.W))))
//  })
//
//  val coefPosition = Wire(Vec(tLen, UInt(symbWidth.W)))
//  val coefPositionPower = Wire(Vec(tLen, UInt(symbWidth.W)))
//
//  for(i <- 0 until tLen) {
//    coefPosition(i) := nLen-1-io.posArray.pos(i) // 255-1-115 = 139
//    coefPositionPower(i) := powerFirstRoot(coefPosition(i)) // 66
//  }
//
//
//  /////////////////////////////////////////
//  // Errata locator
//  /////////////////////////////////////////
//
//  val prePipe = Wire(Vec(tLen, new ErrataLocBundle))
//  val postPipe = Wire(Vec(tLen-1, new ErrataLocBundle))
//  val prePipeVld = Wire(Vec(tLen, Bool()))
//  val postPipeVld = Wire(Vec(tLen-1, Bool()))
//
//  // Errata locator prePipe(0).errataLoc always has a value:
//  // prePipe.errataLoc(0) = [1, coefPosition(0), 0,0,0,...0]
//
//  prePipe(0).errataLoc(0) := 1.U
//  prePipe(0).errataLoc(1) := coefPositionPower(0)
//  for(symbIndx <- 2 until tLen+1) {
//    prePipe(0).errataLoc(symbIndx) := 0.U
//  }
//
//  val stage = for(i <- 1 until tLen) yield Module(new ErrataLocatorStage())
//
//  for(vectIndx <- 1 until tLen) {
//    stage(vectIndx-1).io.errataLocPrev := postPipe(vectIndx-1).errataLoc
//    stage(vectIndx-1).io.coefPositionPower := coefPositionPower(vectIndx)
//    prePipe(vectIndx).errataLoc := stage(vectIndx-1).io.errataLoc
//  }
//
//  for(i <- 0 until tLen) {
//    if(i == 0)
//      prePipeVld(i) := io.posArray.valid
//    else
//      prePipeVld(i) := postPipeVld(i-1)
//  }
//
//  /////////////////////////////////////////
//  // Pipeline
//  /////////////////////////////////////////  
//  
//  val pipe = Module(new pipeline(new ErrataLocBundle, ffStepErrataLocator, tLen, true))
//  pipe.io.prePipe := prePipe
//  pipe.io.prePipeVld.get := prePipeVld 
//  postPipe := pipe.io.postPipe
//  postPipeVld := pipe.io.postPipeVld.get
//
//  /////////////////////////////////////////
//  // Mux 
//  /////////////////////////////////////////  
//
//  val errPosSelPrio = Wire(UInt(log2Ceil(tLen).W))
//  errPosSelPrio := 0.U
//  for(i <- (0 until tLen)) {
//    when(io.posArray.sel(i) === 1) {
//      errPosSelPrio := i.U
//    }
//  }
//
//  io.errataLoc.bits := RegNext(prePipe(errPosSelPrio).errataLoc)
//  io.errataLoc.valid := RegNext(next=prePipeVld(errPosSelPrio), init=false.B)
//
//}
//

class ErrataLocatorStage extends Module with GfParams{
  val io = IO(new Bundle {
    val errataLocPrev = Input(Vec(tLen+1, UInt(symbWidth.W)))
    val errPos = Input(UInt(symbWidth.W))
    val errataLoc = Output(Vec(tLen+1, UInt(symbWidth.W)))
  })

  val coefPosition = Wire(UInt(symbWidth.W))
  val coefPositionPower = Wire(UInt(symbWidth.W))

  coefPosition := nLen-1-io.errPos
  coefPositionPower := powerFirstRoot(coefPosition)

  for(symbIndx <- 0 until tLen+1) {
    if(symbIndx == 0)
      io.errataLoc(symbIndx) := 1.U
    else if(symbIndx == tLen)
      io.errataLoc(symbIndx) := gfMult(io.errataLocPrev(symbIndx-1), coefPositionPower)
    else
      io.errataLoc(symbIndx) := gfMult(io.errataLocPrev(symbIndx-1), coefPositionPower) ^ io.errataLocPrev(symbIndx)
  }
}

// runMain Rs.GenForney
object GenForney extends App {
  //ChiselStage.emitSystemVerilogFile(new ErrataLocatorPar(), Array())
  //ChiselStage.emitSystemVerilogFile(new ErrorEvaluator(), Array())
  ChiselStage.emitSystemVerilogFile(new RsForney(), Array())
}
