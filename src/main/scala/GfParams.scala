package Rs

import chisel3._
import scala.collection.mutable.ArrayBuffer
import chisel3.util.{log2Ceil}

trait GfParams {

  val symbWidth = 8
  val symbNum = 1 << symbWidth
  val poly = 285
  val fieldChar = (1 << symbWidth)-1
  val firstRootPower = 1

  val nLen = 255
  val kLen = 239
  val redundancy = nLen - kLen
  val tLen = (redundancy/2).toInt

  //////////////////////////////
  // Chien parameterization
  //////////////////////////////
  
  val chienRootsPerCycle = 16
  val chienRootsNum = symbNum - 2
  val chienNonValid = ((1 << (chienRootsNum % chienRootsPerCycle)) -1).U
  val chienCyclesNum = calcChienCyclesNum(chienRootsNum, chienRootsPerCycle)
  val chienCntrWidth = log2Ceil(chienCyclesNum)

  val ffStepPosToNum = 4
  val ffStepPolyEval = 4
  //val ffNumPolyEVal = (tLen/ffStepPolyEval).toInt-1

  //////////////////////////////
  // Forney parameterization
  //////////////////////////////

  val ffStepErrataLocator = 2
  val numOfErrataLocStages = 2

  //////////////////////////////
  // Bundles
  //////////////////////////////
  class ErrataLocBundle extends Bundle {
    val errataLoc = Vec(tLen+1, UInt(symbWidth.W))
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
    val errLocator = Vec(tLen, UInt(symbWidth.W))
    val errLocatorSel = UInt(tLen.W)
  }

  def calcChienCyclesNum (rootsNum: Int, rootsPerCycle: Int): Int = {
    if(rootsNum % rootsPerCycle == 0)
      (rootsNum/rootsPerCycle).toInt
    else
      (rootsNum/rootsPerCycle).toInt + 1
  }

  def ohToNum(ohPos: UInt, base: UInt) : UInt = {
    val baseArray = Wire(Vec(chienRootsPerCycle, UInt(symbWidth.W)))
    val baseArrayAndSel = Wire(Vec(chienRootsPerCycle, UInt(symbWidth.W)))
    for(i <- 0 until chienRootsPerCycle) {
      baseArray(i) := base + i.U
      when(ohPos(i) === 1.U) {
        baseArrayAndSel(i) := baseArray(i)
      }.otherwise{
        baseArrayAndSel(i) := 0.U
      }
    }
    baseArrayAndSel.reduce(_ | _)
  }

  //////////////////////////////
  // Forney parameterization
  //////////////////////////////

  //////////////////////////////
  // Block parameterization
  //////////////////////////////
  
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

  def powerFirstRoot(powOfSymb: UInt) : UInt = {
    val powerFirstRootTbl = VecInit(genPowerFirstRoot().map(_.U))
    val powFirstRoot = alphaToSymb(powerFirstRootTbl(powOfSymb))
    powFirstRoot
  }

  //val alpha_to_symb_tbl = VecInit(genAlphaToSymb(symbWidth, poly).map(_.U))
  //val symb_to_alpha_tbl = VecInit(genSymbToAlpha(symbWidth, poly, genAlphaToSymb(symbWidth, poly)).map(_.U))

}
