// TODO: eval latency for each block and create a param.

package Rs

import chisel3._
import scala.collection.mutable.ArrayBuffer
import chisel3.util.{log2Ceil}

trait GfParams {

  //////////////////////////////
  // Code params
  //////////////////////////////

  val symbWidth = 8
  val symbNum = 1 << symbWidth
  val poly = 285
  val fieldChar = symbNum-1
  val firstConsecutiveRoot = 0
  // 
  val firstRootPower = 1


  val nLen = 255
  val kLen = 239
  val redundancy = nLen - kLen
  val tLen = redundancy/2

  //////////////////////////////
  // Input bus parameterization
  //////////////////////////////
  
  val axisWidth = 4
  val msgDuration = nLen/axisWidth

  //////////////////////////////
  // Berlekamp Massey parameterization
  //////////////////////////////

  val numOfSymbBm = 1

  //////////////////////////////
  // Chien parameterization
  //////////////////////////////

  val chienRootsPerCycle = 16
  val chienHorner = true
  val chienHornerComboLen = 2
  // num of stage to eval GF Poly in point 'x' equals to tLen
  require(chienHornerComboLen <= tLen-1, "")

  val chienPosToNumComboLen = 1
  val chienPosToQStages = (tLen-1)/chienPosToNumComboLen
  val ffStepPolyEval = 1
  
  //////////////////////////////
  // Forney parameterization
  //////////////////////////////

  val ffStepErrataLocator = 2
  val numOfErrataLocStages = 2
  // FD - formal derivative params
  val numOfStagesFd0 = 3
  val numOfCyclesFd0 = math.ceil(tLen/numOfStagesFd0.toDouble).toInt
  val cntrStopLimitFd0 = numOfStagesFd0 * numOfCyclesFd0 // 8
  val cntrEopFd0  = numOfStagesFd0 * (numOfCyclesFd0-1)
  val numOfComboLenFd1 = 3
  val numOfQStagesFd1 = (tLen-1)/numOfComboLenFd1

  // ErrEval
  val numOfSymbEe = 3

  // ErrEvalXlInv
  val numOfStagesEe0 = 3
  val numOfComboLenEe0 = 3

  // ErrVal
  val numOfSymbEv = 3

  require(numOfComboLenEe0 <= tLen-1, "Ee0 Combo length more than (tLen-1)")
  val numOfQStagesEe0 = if(numOfComboLenEe0 == tLen-1) 1 else (tLen-1)/numOfComboLenEe0+1
  
  //////////////////////////////
  // Bundles
  //////////////////////////////
  class ErrataLocBundle extends Bundle {
    val errataLoc = Vec(tLen+1, UInt(symbWidth.W))
  }

  class DataSelBundle(width: Int) extends Bundle {
    val data = (Vec(width, UInt(symbWidth.W)))
    val sel  = UInt(width.W)
  }

  class NumPosIf extends Bundle {
    val valid = Bool()
    val sel   = UInt(tLen.W)
    val pos   = Vec(tLen, UInt(symbWidth.W))
  }

  class BitPosIf extends Bundle {
    val valid = Bool()
    val last  = Bool()
    val pos   = UInt(chienRootsPerCycle.W)
  }

  class PosBaseLastVld extends Bundle {
    val pos = UInt(chienRootsPerCycle.W)
    val base = UInt(symbWidth.W)
    //val valid = Bool()
    val last  = Bool()
  }

  class PosBaseVld extends Bundle {
    val pos = UInt(chienRootsPerCycle.W)
    val base = UInt(symbWidth.W)
    val valid = Bool()
  }

  class ErrLocatorBundle extends Bundle {
    val errLocator = Vec(tLen+1, UInt(symbWidth.W))
    val errLocatorSel = UInt((tLen+1).W)
  }

  class ErrLocatorBundle0 extends Bundle {
    val errLocator = Vec(tLen, UInt(symbWidth.W))
    val errLocatorSel = UInt((tLen).W)
  }

  //////////////////////////
  // Forney bundles
  //////////////////////////

  class axisIf(width: Int) extends Bundle {
    val tdata = Vec(width, UInt(symbWidth.W))
    val tkeep = UInt(width.W)
    val tlast = Bool()
  }

  class vecFfsIf(width: Int) extends Bundle {
    val vec = Vec(width, UInt(symbWidth.W))
    val ffs = UInt(width.W)
  }

  class dataSelIf(width: Int) extends Bundle {
    val data = (Vec(width, UInt(symbWidth.W)))
    val sel  = UInt(width.W)
  }


  def calcChienCyclesNum (rootsNum: Int, rootsPerCycle: Int): Int = {
    if(rootsNum % rootsPerCycle == 0)
      (rootsNum/rootsPerCycle).toInt
    else
      (rootsNum/rootsPerCycle).toInt + 1
  }

  //////////////////////////////
  // GF functions
  //////////////////////////////

  def genAlphaToSymb (symbWidth: Int, poly: Int) : Seq[Int] = {
    val alpha_to_symb = new ArrayBuffer[Int](1 << symbWidth)
    alpha_to_symb += 1
    for(i <- 1 until (1 << symbWidth)) {
      if((alpha_to_symb(i-1) & (1 << (symbWidth-1))) != 0)
        alpha_to_symb += (alpha_to_symb(i-1) << 1) ^ poly
      else
        alpha_to_symb += (alpha_to_symb(i-1) << 1)
      //println(alpha_to_symb(i))
    }
    alpha_to_symb.toSeq
  }

  def genSymbToAlpha (symbWidth: Int, poly: Int, alphaToSymbTbl: Seq[Int]) : Seq[Int] = {
    val symbToAlphaTbl = ArrayBuffer.fill(1 << symbWidth)(0)
    for(i <- 0 until (1 << symbWidth)-1) {
      symbToAlphaTbl(alphaToSymbTbl(i)) = i
    }
    symbToAlphaTbl.toSeq
  }

  def alphaToSymb(alpha: UInt): UInt = {
    val alphaToSymbTbl = VecInit(genAlphaToSymb(symbWidth, poly).map(_.U))
    val symb = Wire(UInt(symbWidth.W))
    symb := alphaToSymbTbl(alpha)
    symb
  }

  def symbToAlpha(symb: UInt): UInt = {
    val symbToAlphaTbl = VecInit(genSymbToAlpha(symbWidth, poly, genAlphaToSymb(symbWidth, poly)).map(_.U))
    val alpha = Wire(UInt(symbWidth.W))
    alpha := symbToAlphaTbl(symb)
    alpha
  }

  def gfMult (symbA: UInt, symbB: UInt) : UInt = {
    val mult = Wire(UInt(symbWidth.W))
    val alphaSum = Wire(UInt(symbWidth.W))
    val alphaA = Wire(UInt(symbWidth.W))
    val alphaB = Wire(UInt(symbWidth.W))
    alphaA := symbToAlpha(symbA)
    alphaB := symbToAlpha(symbB)
    alphaSum := (alphaA + alphaB) % fieldChar.U
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
    val alphaDiff = (fieldChar.U.asTypeOf(UInt((symbWidth+1).W))+alphaDivd-alphaDvdr)%fieldChar.U
    val gfDivVal = Wire(UInt(symbWidth.W))
    when(dividend === 0.U) {
      gfDivVal := 0.U
    }.otherwise {
      gfDivVal := alphaToSymb(alphaDiff)
    }
    gfDivVal
  }
  
  def gfInv(symb : UInt) : UInt = {
    val alphaInv = Wire(UInt(symbWidth.W))
    alphaInv := (symbNum - 1).U - symbToAlpha(symb)
    val gfInverse =alphaToSymb(alphaInv)
    gfInverse
  }

  def gfPow(x : UInt, degree : UInt) : UInt = {
    val alpha = symbToAlpha(x)
    val alphaPow = (alpha * degree) % fieldChar.U
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
  // GF poly operations 
  ////////////////////////////////////////////

  // TODO: simplify if firstRoot == 2(firstRootPower = 1)
  // then:
  // genPowerFirstRootTbl += i % fieldChar
  def genPowerFirstRoot() : Seq[Int] = {
    val genPowerFirstRootTbl = new ArrayBuffer[Int](nLen)
    for(i <- 0 until nLen) {
      genPowerFirstRootTbl += (firstRootPower * i) % fieldChar
    }
    genPowerFirstRootTbl.toSeq
  }

  def genPowerFirstRootMin1() : Seq[Int] = {
    val genPowerFirstRootTbl = new ArrayBuffer[Int](nLen)
    for(i <- 0 until nLen) {
      genPowerFirstRootTbl += ((firstRootPower-1) * i) % fieldChar
    }
    genPowerFirstRootTbl.toSeq
  }

  def genPowerFirstRootNeg() : Seq[Int] = {
    val genPowerFirstRootNegTbl = new ArrayBuffer[Int](nLen)
    for(i <- 0 until nLen) {
      genPowerFirstRootNegTbl += (firstRootPower*(fieldChar-i)) % fieldChar
    }
    genPowerFirstRootNegTbl.toSeq
  }

  def powerFirstRoot(powOfSymb: UInt) : UInt = {
    val powerFirstRootTbl = VecInit(genPowerFirstRoot().map(_.U))
    val powFirstRoot = alphaToSymb(powerFirstRootTbl(powOfSymb))
    powFirstRoot
  }

  def powerFirstRootMin1(powOfSymb: UInt) : UInt = {
    val powerFirstRootMin1Tbl = VecInit(genPowerFirstRootMin1().map(_.U))
    val powFirstRootMin1 = alphaToSymb(powerFirstRootMin1Tbl(powOfSymb))
    powFirstRootMin1
  }

  def powerFirstRootNeg(powOfSymb: UInt) : UInt = {
    val powerFirstRootNegTbl = VecInit(genPowerFirstRootNeg().map(_.U))
    val powFirstRootNeg = alphaToSymb(powerFirstRootNegTbl(powOfSymb))
    powFirstRootNeg
  }

  //val alpha_to_symb_tbl = VecInit(genAlphaToSymb(symbWidth, poly).map(_.U))
  //val symb_to_alpha_tbl = VecInit(genSymbToAlpha(symbWidth, poly, genAlphaToSymb(symbWidth, poly)).map(_.U))

}
