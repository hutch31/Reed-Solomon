package Rs

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._

class DeleteItem(vectorWidth: Int, itemWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(vectorWidth, UInt(itemWidth.W)))
    val sel = Input(UInt(log2Ceil(vectorWidth).W))
    val out = Output(Vec(vectorWidth-1, UInt(itemWidth.W)))
  })

  for(row <- 0 until vectorWidth-1) {
    val muxIn = Wire(Vec(vectorWidth, UInt(itemWidth.W)))
    for(column <- 0 until vectorWidth) {
      if(row < column)
        muxIn(column) := io.in(row)
      else
        muxIn(column) := io.in(row+1)
    }
    io.out(row) := muxIn(io.sel)
  }

}

// runMain Rs.GenDeleteItem
object GenDeleteItem extends App {
  ChiselStage.emitSystemVerilogFile(new DeleteItem(8,8), Array())
}
