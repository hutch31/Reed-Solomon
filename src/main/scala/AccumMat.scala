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
import chisel3.util._

class AccumMat(symbWidth: Int, vecWidth: Int, vecNum : Int, pipeWidth: Int, matCol: Int) extends Module {

  require(matCol <= pipeWidth*vecNum, "[ERROR] Matrix accumulator.")
  val io = IO(new Bundle {
    val vecIn = Input(Vec(vecNum, (Vec(vecWidth, UInt(symbWidth.W)))))
    val matPipeOut = Output(Vec(pipeWidth, (Vec(vecNum, (Vec(vecWidth, UInt(symbWidth.W)))))))
    val matOut = Output(Vec(matCol, (Vec(vecWidth, UInt(symbWidth.W)))))
    val matTOut = Output(Vec(vecWidth, (Vec(matCol, UInt(symbWidth.W)))))
  })

  val pipeVecQ = Reg(Vec(pipeWidth, (Vec(vecNum, (Vec(vecWidth, UInt(symbWidth.W)))))))
  val mat = Wire(Vec(matCol, (Vec(vecWidth, UInt(symbWidth.W)))))

  for(i <- 0 until pipeWidth) {
    for(k <- 0 until vecNum) {
      if(i == 0)
        pipeVecQ(pipeWidth-1)(k) := io.vecIn(k)
      else
        pipeVecQ(pipeWidth-1-i)(k) := pipeVecQ(pipeWidth-i)(k)
      if(i*vecNum+k < matCol)
        mat(i*vecNum+k) := pipeVecQ(i)(k)
    }    
  }

  // Transpose matrix

  for(i <- 0 until matCol) {
    for(k <- 0 until vecWidth) {
      io.matTOut(k)(i) := mat(i)(k)
    }
  }

  io.matOut := mat
  io.matPipeOut := pipeVecQ

}
