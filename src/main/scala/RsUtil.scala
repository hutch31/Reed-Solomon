package Rs
import chisel3._
import chisel3.util.log2Ceil

object RsUtil {
  def modulo(a : UInt, b : Int) : UInt = Mux(a >= b.U, a - b.U, a)

  def const_mul(x: UInt, mul: Int): UInt = {
    require(mul >= 0)
    if (mul > 0) {
      var shift: Int = 0
      var m: Int = mul
      val width = x.getWidth + log2Ceil(mul)
      val mstage = Wire(Vec(log2Ceil(mul + 1), UInt(width.W)))
      while (m > 0) {
        if ((m & 1) == 1) {
          mstage(shift) := x << shift
        } else {
          mstage(shift) := 0.U
        }
        shift += 1
        m = m >> 1
      }
      mstage.reduce(_ + _)
    } else 0.U
  }
}
