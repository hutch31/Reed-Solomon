/*
 * ----------------------------------------------------------------------
 *  Copyright (c) 2024 Egor Smirnov
 *
 *  Licensed under terms of the MIT license
 *  See https://github.com/egorman44/Reed-Solomon/blob/main/LICENSE
 *    for license terms
 * ----------------------------------------------------------------------
 */

// TODO: Check usage (_||_) vs (_|_)
// TODO: rename symb-width > symb-width-bits and bus-width > bus-width-bytes
package Rs

import chisel3._
import scala.collection.mutable.ArrayBuffer
import chisel3.util.{log2Ceil}
import scala.math.floor
import play.api.libs.json._
import scala.io.Source

case class Config(
  cmdCfg: CmdConfig
)
{
  val AXIS_CLOCK   = cmdCfg.AXIS_CLOCK
  val CORE_CLOCK   = cmdCfg.CORE_CLOCK
  val SYMB_WIDTH   = cmdCfg.SYMB_WIDTH
  val BUS_WIDTH    = cmdCfg.BUS_WIDTH
  val POLY         = cmdCfg.POLY
  val FCR          = cmdCfg.FCR
  val N_LEN        = cmdCfg.N_LEN
  val K_LEN        = cmdCfg.K_LEN
  val REDUNDANCY   = N_LEN - K_LEN
  val T_LEN        = REDUNDANCY/2
  val SYMB_NUM     = 1 << SYMB_WIDTH
  val FIELD_CHAR   = SYMB_NUM-1
  val MSG_DURATION = math.ceil(N_LEN/BUS_WIDTH.toDouble).toInt
  val CLOCK_RATION = (AXIS_CLOCK/CORE_CLOCK).toDouble
  val decoderSingleClock = if(CLOCK_RATION == 1.0) true else false
  val MSG_DURATION_CORE = math.ceil(MSG_DURATION/CLOCK_RATION).toInt

  def calcNumStages(comboLen: Int, pipelineInterval: Int): Int = {
    require(comboLen > 0 && pipelineInterval > 0,
      s"Both comboLen ($comboLen) and pipelineInterval ($pipelineInterval) must be positive."
    )
    (comboLen + pipelineInterval - 1) / pipelineInterval
  }

  def computeTermsAndLatency(LEN: Int, MSG_DURATION_CORE: Int, Incr: Int=1): (Int, Int) = {
    var TermsPerCycle = 0 // The number of terms the shift module produces in each cycle
    var ShiftLatency = 0  // The number of cycles it takes the shift module to complete a shift operation
    do {
      TermsPerCycle += Incr
      ShiftLatency = math.ceil(LEN / TermsPerCycle.toDouble).toInt
    } while (MSG_DURATION_CORE < ShiftLatency)

    (TermsPerCycle, ShiftLatency)
  }

  ///////////////////////////////////////////
  // Syndrome
  ///////////////////////////////////////////

  val syndPipeEn = true

  ///////////////////////////////////////////
  // Berlekamp Massey Parameters
  ///////////////////////////////////////////

  // bmTermsPerCycle - defines the number of syndrome terms BM can process in a cycle(calculated sequentially)
  val bmTermsPerCycle = 1
  val bmStagePipeEn = false
  val bmPipeMult = if(bmStagePipeEn) 2 else 1
  val bmShiftLimit = if(bmStagePipeEn) 1 else 0
  val bmShiftLatency = math.ceil(REDUNDANCY/bmTermsPerCycle.toDouble).toInt * bmPipeMult
  val bmLatencyFull = bmShiftLatency + 2 // shiftBundle capture + shiftBundle shift + output reg

  var rsBmNum = 1

  while(MSG_DURATION < math.ceil(bmShiftLatency/rsBmNum.toDouble).toInt) {
    rsBmNum += 1
  }

  ///////////////////////////////////////////
  // Chien search parameters:
  //
  // chienErrBitPosTermsPerCycle - defines number of roots generated by RsChienErrBitPos block
  ///////////////////////////////////////////

  // chienPosToNumComboLen - this parameter is used for pipelining the block. This parameter determines after how many stages a register is inserted.
  val chienPosToNumComboLen = 1
  val chienBitPosLatency = if(chienPosToNumComboLen == T_LEN-1) 0 else (T_LEN-1)/chienPosToNumComboLen

  val chienHorner = false // TODO: add substitution for Horner PolyEval
  val(chienErrBitPosTermsPerCycle, chienErrBitPosShiftLatency) = computeTermsAndLatency(N_LEN, MSG_DURATION_CORE, 4)

  val chienBitPosPipeIntrvl = 10
  val chienBitPosPipeNumStages = calcNumStages(chienErrBitPosTermsPerCycle, chienBitPosPipeIntrvl)
  val chienBitPosLatencyFull = chienBitPosPipeNumStages + 1 + 1 + math.ceil(FIELD_CHAR/chienErrBitPosTermsPerCycle).toInt

  val chienErrBitPosLatencyFull = chienErrBitPosShiftLatency + 1
  val chienLatencyFull = chienBitPosLatencyFull + chienErrBitPosLatencyFull

  ///////////////////////////////////////////
  // Forney algorithm parameters
  ///////////////////////////////////////////

  ///////////////
  // ErrataLoc
  //
  // ErrataLoc is implemented as a feedback accumulator with
  // forneyErrataLocTermsPerCycle = 1, to relax timing.
  ///////////////
  
  //val (forneyErrataLocTermsPerCycle, forneyErrataLocShiftLatency) = computeTermsAndLatency(T_LEN, MSG_DURATION_CORE)
  val forneyErrataLocTermsPerCycle = 1
  val forneyErrataLocShiftLatency = math.ceil(T_LEN / forneyErrataLocTermsPerCycle.toDouble).toInt
  val forneyErrataLocLatencyFull = forneyErrataLocShiftLatency + 1 // +1 output reg

  var errataLocNum = 1

  while(MSG_DURATION_CORE < math.ceil(forneyErrataLocLatencyFull/errataLocNum.toDouble).toInt) {
    errataLocNum += 1
  }

  ///////////////
  // ErrEval
  ///////////////  
  
  // forneyErrEvalTermsPerCycle - defines number of errata locator terms ErrEval block can process in a cycle(calculated in parallel)
  val (forneyErrEvalTermsPerCycle,forneyErrEvalShiftLatency) = computeTermsAndLatency(T_LEN+1, MSG_DURATION_CORE)
  val forneyErrEvalLatencyFull = forneyErrEvalShiftLatency + 3 // +1 accumVld +1 syndXErrataLocVld +1 errEvalIf.valid

  ///////////////
  // ErrEvalXlInv
  ///////////////  

  // forneyEEXlInvTermsPerCycle - defines number of XlInv terms ErrEvalXlInv block can process in a cycle(calculated in parallel)
  val (forneyEEXlInvTermsPerCycle, forneyEEXlInvShiftLatency) = computeTermsAndLatency(T_LEN, MSG_DURATION_CORE)
  // forneyEEXlInvComboLen - this parameter is used for pipelining the stage of ErrEvalXlInv block. This parameter determines after how many stages a register is inserted.
  val forneyEEXlInvComboLen = 1
  //require(forneyEEXlInvComboLen <= T_LEN-1, "ErrEvalXlInvStage combo length more than (T_LEN-1)")
  val forneyEEXlInvQStages = math.ceil((T_LEN)/forneyEEXlInvComboLen.toDouble).toInt
  val forneyEEXlInvShiftLatencyFull = forneyEEXlInvQStages + forneyEEXlInvShiftLatency + 2 // +1 accumVld +1 output reg

  ///////////////
  // formalderivative
  ///////////////  

  // forneyFdTermsPerCycle - defines number of XlInv terms FormalDerivative block can process in a cycle(calculated in parallel)
  val (forneyFdTermsPerCycle, forneyFdShiftLatency) = computeTermsAndLatency(T_LEN, MSG_DURATION_CORE)
  val forneyFdStopLimit = forneyFdTermsPerCycle * forneyFdShiftLatency // 8
  val forneyFdEop  = forneyFdTermsPerCycle * (forneyFdShiftLatency-1)
  val forneyFdComboLen = 2
  val forneyFdQStages = math.ceil(T_LEN-1/forneyFdComboLen.toDouble).toInt

  val forneyFdFullLatency = forneyFdShiftLatency + forneyFdQStages + 2 // +1 accumVld +1 output reg

  ///////////////
  // ErrVal
  ///////////////  

  val (forneyEvTermsPerCycle,forneyEvShiftLatency)   = computeTermsAndLatency(T_LEN, MSG_DURATION_CORE)
  val forneyEvFullLatency = forneyEvShiftLatency + 1 // +1 accum

  val decoderLatencyFull = bmLatencyFull + chienErrBitPosLatencyFull + chienBitPosLatencyFull + forneyErrataLocLatencyFull + forneyErrEvalLatencyFull + forneyEEXlInvShiftLatencyFull + forneyFdFullLatency + forneyEvFullLatency
  // Eval latency relative to AXIS clock
  val decoderLatencyFullAxisClk = decoderLatencyFull * CLOCK_RATION

  val msgNum = math.ceil((MSG_DURATION + decoderLatencyFullAxisClk)/MSG_DURATION.toDouble).toInt + 1 // +1 to make sure it's enough

  ///////////////////////////////////////////
  // Check if FIFOs required in Forney
  // block to synchronize data flow
  ///////////////////////////////////////////

  // If the latency in between RsSyndrome output and ErrEval block > MSG_DURATION,
  // then syndrome value will be updated before it's used in ErrEval block.
  // In this case syndrome FIFO inserted in the Forney block.
  val syndErrEvalLatency = bmLatencyFull + chienLatencyFull + forneyErrataLocLatencyFull + forneyErrEvalLatencyFull
  val syndErrEvalLatencyAxisClk = (math.ceil(syndErrEvalLatency * CLOCK_RATION)).toInt

  val forneySyndFifoEn = if(syndErrEvalLatencyAxisClk > MSG_DURATION || !decoderSingleClock) true else false
  val forneySyndFifoDepth = math.ceil(syndErrEvalLatencyAxisClk/MSG_DURATION.toDouble).toInt+1 // +1 to make sure it's enough
  
  // XlInvIf FIFO to EEXlInv block
  val XlInvIfToEEXlInvLatencyAxisClk = (math.ceil((forneyErrataLocLatencyFull+forneyErrEvalLatencyFull) * CLOCK_RATION)).toInt
  val XlInvIfToEEXlInvFifoEn = if (XlInvIfToEEXlInvLatencyAxisClk >= MSG_DURATION) true else false
  val XlInvIfToEEXlInvFifoDepth = math.ceil(XlInvIfToEEXlInvLatencyAxisClk/MSG_DURATION.toDouble).toInt+1

  // XlIf FIFO to errVal block
  val XlIfToEvLatencyAxisClk = (math.ceil((forneyErrataLocLatencyFull+forneyErrEvalLatencyFull+forneyEEXlInvShiftLatencyFull) * CLOCK_RATION)).toInt
  val XlIfToEvFifoEn = if (XlIfToEvLatencyAxisClk >= MSG_DURATION) true else false
  val XlIfToEvFifoDepth = math.ceil(XlIfToEvLatencyAxisClk/MSG_DURATION.toDouble).toInt+1

  // formalDer FIFO to errVal block
  val FdToEvLatencyAxisClk = (math.ceil((forneyErrataLocLatencyFull+forneyErrEvalLatencyFull+forneyEEXlInvShiftLatencyFull-forneyFdFullLatency) * CLOCK_RATION)).toInt
  val FdToEvFifoEn = if (FdToEvLatencyAxisClk >= MSG_DURATION) true else false
  val FdToEvFifoDepth = math.ceil(FdToEvLatencyAxisClk/MSG_DURATION.toDouble).toInt+1

  println(s"=== RS code config === ")
  println(s"N_LEN                         = $N_LEN")
  println(s"K_LEN                         = $K_LEN")
  println(s"REDUNDANCY                    = $REDUNDANCY")
  println(s"T_LEN                         = $T_LEN")
  println(s"POLY                          = $POLY")
  println(s"=== Decoder config === ")
  println(s"AXIS_CLOCK                    = $AXIS_CLOCK")
  //println(s"CORE_CLOCK                  = $CORE_CLOCK")
  //println(s"CLOCK_RATION                = $CLOCK_RATION")
  println(s"decoderSingleClock            = $decoderSingleClock")
  println(s"MSG_DURATION                  = $MSG_DURATION")
  //println(s"MSG_DURATION_CORE           = $MSG_DURATION_CORE")
  println(s"Decoder latency               = $decoderLatencyFull")
  println(s"=== BM ===")
  println(s"bmTermsPerCycle               = $bmTermsPerCycle")
  println(s"bmStagePipeEn                 = $bmStagePipeEn")
  println(s"bmLatencyFull                 = $bmLatencyFull")
  println(s"rsBmNum                       = $rsBmNum")
  println(s"=== CHIEN ===")
  println(s"chienErrBitPosTermsPerCycle   = $chienErrBitPosTermsPerCycle")
  println(s"chienErrBitPosLatencyFull     = $chienErrBitPosLatencyFull")
  println(s"chienBitPosLatencyFull        = $chienBitPosLatencyFull")
  println(s"chienLatencyFull              = $chienLatencyFull")
  println(s"=== FORNEY ===")
  println(s"forneyFdTermsPerCycle         = $forneyFdTermsPerCycle")
  println(s"forneyFdFullLatency           = $forneyFdFullLatency")
  println(s"forneyErrataLocTermsPerCycle  = $forneyErrataLocTermsPerCycle")
  println(s"forneyErrataLocLatencyFull    = $forneyErrataLocLatencyFull")
  println(s"errataLocNum                  = $errataLocNum")
  println(s"forneyErrEvalTermsPerCycle    = $forneyErrEvalTermsPerCycle")
  println(s"forneyErrEvalLatencyFull      = $forneyErrEvalLatencyFull")
  println(s"forneyEEXlInvTermsPerCycle    = $forneyEEXlInvTermsPerCycle")
  println(s"forneyEEXlInvShiftLatencyFull = $forneyEEXlInvShiftLatencyFull")
  println(s"forneyEvTermsPerCycle         = $forneyEvTermsPerCycle")
  println(s"forneyEvFullLatency           = $forneyEvFullLatency")
  println(s"FdToEvLatencyAxisClk          = $FdToEvLatencyAxisClk")
  println(s"=== FIFO ENABLE ===")
  var fifoDbg: String = if (forneySyndFifoEn) s"true($forneySyndFifoDepth)" else "false"
  println(s"SyndIf  -> errEval: $fifoDbg")
  fifoDbg = if (XlInvIfToEEXlInvFifoEn) s"true($XlInvIfToEEXlInvFifoDepth)" else "false"
  println(s"XlInvIf -> EEXlEnv: $fifoDbg")
  fifoDbg = if (FdToEvFifoEn) s"true($FdToEvFifoDepth)" else "false"
  println(s"FdIf    -> errVal : $fifoDbg")
  fifoDbg = if (XlIfToEvFifoEn) s"true($XlIfToEvFifoDepth)" else "false"
  println(s"XlIf    -> errVal : $fifoDbg")
  println(s"FIFO should store $msgNum messages.")

  //////////////////////////////
  // GF arithmetic
  //////////////////////////////

  /////////////////////////////
  // The table maps powers of the primitive element ("alpha")
  // to their corresponding symbol values:
  // alpha_to_symb[0] = alpha ^ 0 = 1
  // alpha_to_symb[1] = alpha ^ 1 = 2
  /////////////////////////////

  def genAlphaToSymb() : Seq[Int] = {
    val alpha_to_symb = new ArrayBuffer[Int](1 << SYMB_WIDTH)
    alpha_to_symb += 1
    for(i <- 1 until (1 << SYMB_WIDTH)) {
      if((alpha_to_symb(i-1) & (1 << (SYMB_WIDTH-1))) != 0)
        alpha_to_symb += (alpha_to_symb(i-1) << 1) ^ POLY
      else
        alpha_to_symb += (alpha_to_symb(i-1) << 1)
    }
    alpha_to_symb.toSeq
  }

  def genSymbToAlpha (alphaToSymbTbl: Seq[Int]) : Seq[Int] = {
    val symbToAlphaTbl = ArrayBuffer.fill(1 << SYMB_WIDTH)(0)
    for(i <- 0 until (1 << SYMB_WIDTH)-1) {
      symbToAlphaTbl(alphaToSymbTbl(i)) = i
    }
    symbToAlphaTbl.toSeq
  }


  def alphaToSymb(alpha: UInt): UInt = {
    val alphaToSymbTbl = VecInit(genAlphaToSymb().map(_.U))
    val symb = Wire(UInt(SYMB_WIDTH.W))
    symb := alphaToSymbTbl(alpha)
    symb
  }

  def symbToAlpha(symb: UInt): UInt = {
    val symbToAlphaTbl = VecInit(genSymbToAlpha(genAlphaToSymb()).map(_.U))
    val alpha = Wire(UInt(SYMB_WIDTH.W))
    alpha := symbToAlphaTbl(symb)
    alpha
  }

  val GENERATOR = 2
  val GENERATOR_POWER = genSymbToAlpha(genAlphaToSymb())(GENERATOR)
  val FCR_SYMB = genAlphaToSymb()(FCR)

  println(s"GENERATOR_POWER = $GENERATOR_POWER")
  println(s"FCR             = $FCR")
  println(s"FCR_SYMB        = $FCR_SYMB")
  
  def gfMult (symbA: UInt, symbB: UInt) : UInt = {
    val mult = Wire(UInt(SYMB_WIDTH.W))
    val alphaSum = Wire(UInt(SYMB_WIDTH.W))
    val alphaA = Wire(UInt(SYMB_WIDTH.W))
    val alphaB = Wire(UInt(SYMB_WIDTH.W))
    alphaA := symbToAlpha(symbA)
    alphaB := symbToAlpha(symbB)
    alphaSum := (alphaA + alphaB) % FIELD_CHAR.U
    when((symbA === 0.U) | (symbB === 0.U)){
      mult := 0.U
    }.otherwise{
      mult := alphaToSymb(alphaSum)
    }
    mult
  }

  def gfDiv (dividend: UInt, divider: UInt) : UInt = {
    val alphaDivd = symbToAlpha(dividend)
    val alphaDvdr = symbToAlpha(divider)
    // TODO: use +& instead of implicit cast.
    val alphaDiff = (FIELD_CHAR.U.asTypeOf(UInt((SYMB_WIDTH+1).W))+alphaDivd-alphaDvdr)%FIELD_CHAR.U
    val gfDivVal = Wire(UInt(SYMB_WIDTH.W))
    when(dividend === 0.U) {
      gfDivVal := 0.U
    }.otherwise {
      gfDivVal := alphaToSymb(alphaDiff)
    }
    gfDivVal
  }
  
  def gfInv(symb : UInt) : UInt = {
    val alphaInv = Wire(UInt(SYMB_WIDTH.W))
    alphaInv := (SYMB_NUM - 1).U - symbToAlpha(symb)
    val gfInverse =alphaToSymb(alphaInv)
    gfInverse
  }

  def gfPow(x : UInt, degree : UInt) : UInt = {
    val alpha = symbToAlpha(x)
    val alphaPow = (alpha * degree) % FIELD_CHAR.U
    val xDegree = alphaToSymb(alphaPow)
    xDegree
  }

  def gfPolyMultX(poly: Vec[UInt]) : Vec[UInt] = {
    val outVec = Wire(poly.cloneType)
    outVec(0) := 0.U
    for(i <- 0 until poly.size-1) {
      outVec(i+1) := poly(i)
    }
    outVec
  }

  ////////////////////////////////////////////
  // gfPow(x=GENERATOR, power)
  ////////////////////////////////////////////

  // TODO: simplify if firstRoot == 2(firstRootPower = 1)
  // then:
  // genPowerFirstRootTbl += i % fieldChar
  def genPowerFirstRoot() : Seq[Int] = {
    val genPowerFirstRootTbl = new ArrayBuffer[Int](SYMB_NUM)
    for(i <- 0 until SYMB_NUM) {
      genPowerFirstRootTbl += (GENERATOR_POWER * i) % FIELD_CHAR
    }
    genPowerFirstRootTbl.toSeq
  }

  def genPowerFirstRootNeg() : Seq[Int] = {
    val genPowerFirstRootNegTbl = new ArrayBuffer[Int](SYMB_NUM)
    for(i <- 0 until SYMB_NUM) {
      genPowerFirstRootNegTbl += (GENERATOR_POWER*(FIELD_CHAR-i)) % FIELD_CHAR
    }
    genPowerFirstRootNegTbl.toSeq
  }

  def powerFirstRootNeg(powOfSymb: UInt) : UInt = {
    val powerFirstRootNegTbl = VecInit(genPowerFirstRootNeg().map(_.U))
    val powFirstRootNeg = alphaToSymb(powerFirstRootNegTbl(powOfSymb))
    powFirstRootNeg
  }

  def genPowerFirstRootMin1() : Seq[Int] = {
    val genPowerFirstRootTbl = new ArrayBuffer[Int](SYMB_NUM)
    for(i <- 0 until SYMB_NUM) {
      genPowerFirstRootTbl += ((GENERATOR_POWER-1) * i) % FIELD_CHAR
    }
    genPowerFirstRootTbl.toSeq
  }

  def powerFirstRootMin1(powOfSymb: UInt) : UInt = {
    val powerFirstRootMin1Tbl = VecInit(genPowerFirstRootMin1().map(_.U))
    val powFirstRootMin1 = alphaToSymb(powerFirstRootMin1Tbl(powOfSymb))
    powFirstRootMin1
  }

  def powerFirstRoot(powOfSymb: UInt) : UInt = {
    val powerFirstRootTbl = VecInit(genPowerFirstRoot().map(_.U))
    val powFirstRoot = alphaToSymb(powerFirstRootTbl(powOfSymb))
    powFirstRoot
  }
}
