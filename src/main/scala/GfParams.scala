package Rs

import chisel3._
import scala.collection.mutable.ArrayBuffer
import chisel3.util.{log2Ceil}

trait GfParams {

  val symbWidth = 8
  val symbNum = 1 << symbWidth
  val poly = 285
  val field_charac = (1 << symbWidth)-1

  val nLen = 255
  val kLen = 239
  val redundancy = nLen - kLen
  val tLen = (redundancy/2).toInt

  //////////////////////////////
  // Chien parameterization
  //////////////////////////////

  val chienRootsPerCycle = 16
  val chienRootsNum = symbNum - 2
  val chienNonValid = chienRootsNum % chienRootsPerCycle
  val chienCyclesNum = calcChienCyclesNum(chienRootsNum, chienRootsPerCycle)
  val chienCntrWidth = log2Ceil(chienCyclesNum)

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

  //////////////////////////////
  // Block parameterization
  //////////////////////////////

  def clog2(x: Int): Int = {
    require(x > 0, "Argument to clog2 must be greater than zero")
      (math.log(x) / math.log(2)).ceil.toInt
  }

  //////////////////////////////
  // Block parameterization
  //////////////////////////////

  val ffStepPolyEval = 4
  val ffNumPolyEVal = (tLen/ffStepPolyEval).toInt-1

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
    alphaSum := (alphaA + alphaB) % field_charac.U
    when((symbA === 0.U) | (symbB === 0.U)){
      mult := 0.U
    }.otherwise{
      mult := alphaToSymb(alphaSum)
    }
    mult
  }



  //val alpha_to_symb_tbl = VecInit(genAlphaToSymb(symbWidth, poly).map(_.U))
  //val symb_to_alpha_tbl = VecInit(genSymbToAlpha(symbWidth, poly, genAlphaToSymb(symbWidth, poly)).map(_.U))

}
