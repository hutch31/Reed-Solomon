package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

class DiagonalXorAll(row: Int, col: Int, elementWidth: Int) extends Module {
  val io = IO(new Bundle {
    val recMatrix = Input(Vec(row, Vec(col, UInt(elementWidth.W))))
    val xorVect = Output(Vec(row+col-1, UInt(elementWidth.W)))
  })

  for(i <- 0 until row+col-1) {
    val diagXor = Module(new DiagonalXor(row, col, i, elementWidth))
    diagXor.io.recMatrix := io.recMatrix
    io.xorVect(i) := diagXor.io.xor
  }

}

class DiagonalXor(row: Int, col: Int, diagIndx: Int, elementWidth: Int) extends Module {
  val io = IO(new Bundle {
    val recMatrix = Input(Vec(row, Vec(col, UInt(elementWidth.W))))    
    val xor = Output(UInt(elementWidth.W))
  })

  val colIntrm = col.min(row)

  var xorEval = 0.U

  for(rowIndx <- 0 until row) {
    for(colIndx <- 0 until col) {  // Corrected to iterate up to 'col' instead of 'row'
      if(rowIndx + colIndx == diagIndx) {
        xorEval = xorEval ^ io.recMatrix(rowIndx)(colIndx)
      }
    }
  }
  
  io.xor := xorEval
}

// runMain Rs.Matrix
object Matrix extends App {
  ChiselStage.emitSystemVerilogFile(new DiagonalXorAll(8,16,8), Array())
}
