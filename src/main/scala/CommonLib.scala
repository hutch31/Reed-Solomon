/*
 * ----------------------------------------------------------------------
 *  Copyright (c) 2024 Egor Smirnov
 *
 *  Licensed under terms of the MIT license
 *  See https://github.com/egorman44/Reed-Solomon/blob/main/LICENSE
 *    for license terms
 * ----------------------------------------------------------------------
 */

package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

class PrioEnc(width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val out = Output(UInt(log2Ceil(width).W))
  })

  io.out := 0.U
  for(i <- (0 until width)) {
    when(io.in(i) === 1) {
      io.out := i.U
    }
  }

}

// TODO: delete FindFirstSet

class FindFirstSet(width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W)) // Assuming a width-bit input vector
    val out = Output(UInt(width.W)) // Output is a one-hot vector
  })

  // Find the index of the first set bit
  val prioEnc = Module(new PrioEnc(width))
  prioEnc.io.in := io.in

  // Generate a one-hot vector
  io.out := UIntToOH(prioEnc.io.out, width = width)
}

class FindFirstSetNew(width: Int, lsbFirst: Boolean=true) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W)) // A width-bit input vector
    val out = Output(UInt(width.W)) // Output is a one-hot vector
  })
  
  io.out := 0.U

  if(lsbFirst) {
    for(i <- (0 until width).reverse) {
      when(io.in(i) === 1.U) {
        io.out := (BigInt(1) << i).U
      }
    }
  } else {
    for(i <- (0 until width)) {
      when(io.in(i) === 1.U) {
        io.out := (BigInt(1) << i).U
      }
    }
  }

}

class OneHotMux(width: Int) extends Module {
  require(width > 0, "Width must be greater than 0")

  val io = IO(new Bundle {
    val in = Input(Vec(width, UInt(8.W))) // Input is a vector of one-hot signals
                                          //val sel = Input(Vec(width, Bool())) // Selection signal
    val sel = Input(UInt(width.W)) // Selection signal
    val out = Output(UInt(8.W)) // Output is the selected one-hot signal
  })

  io.out := Mux1H(io.sel, io.in)
}

// runMain Rs.GenFfs
object GenFfs extends App {
  ChiselStage.emitSystemVerilogFile(new OneHotMux(4), Array())
  //ChiselStage.emitSystemVerilogFile(new FindFirstSet(8), Array())
  //ChiselStage.emitSystemVerilogFile(new ffs(8), Array())
}
