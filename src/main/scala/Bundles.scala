package Rs
import chisel3._
import chisel3.util._

// Bundles
class axisIf(width: Int, symbWidth: Int) extends Bundle {
  val tdata = Vec(width, UInt(symbWidth.W))
  val tkeep = UInt(width.W)
  val tlast = Bool()
}

class vecFfsIf(width: Int, symbWidth: Int) extends Bundle {
  val vec = Vec(width, UInt(symbWidth.W))
  val ffs = UInt(width.W)
}

class BitPosIf(width: Int) extends Bundle {
  val valid = Bool()
  val last  = Bool()
  val pos   = UInt(width.W)
}
