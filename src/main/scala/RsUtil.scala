package Rs
import chisel3._

object RsUtil {
  def modulo(a : UInt, b : Int) : UInt = Mux(a >= b.U, a - b.U, a)

}
