package Rs

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import circt.stage.ChiselStage
import chisel3.util._

// To disable pipelining ffStep == 0 or ffStep >= pipeLen

class pipeline[T <: Data](dt: T, ffStep: Int, pipeLen: Int, vldEn: Boolean ) extends Module {
  val io = IO(new Bundle {
    val prePipe = Input(Vec(pipeLen, dt))
    val postPipe = Output(Vec(pipeLen-1, dt))
    val prePipeVld = if(vldEn) Some(Input(Vec(pipeLen, Bool()))) else None
    val postPipeVld = if(vldEn) Some(Output(Vec(pipeLen-1, Bool()))) else None
  })

  var ffNum = 0
  var ffStepVar = pipeLen

  if(ffStep != 0) {
    ffNum = math.ceil(pipeLen.toDouble/ffStep.toDouble).toInt-1
    ffStepVar = ffStep
  }


  if(vldEn & ffNum != 0) {
    val vldQ = RegInit(VecInit(Seq.fill(ffNum)(0.B)))
    for(i <- 0 until ffNum) {
      vldQ(i) := io.prePipeVld.get((i*ffStepVar)+ffStepVar-1)
      io.postPipeVld.get((i*ffStepVar)+ffStepVar-1) := vldQ(i)
    }
  }

  for(i <- 0 until ffNum) {
    io.postPipe((i*ffStepVar)+ffStepVar-1) := RegNext(io.prePipe((i*ffStepVar)+ffStepVar-1))
  }

  for(i <- 0 until pipeLen-1) {
    if((i+1) % ffStepVar != 0) {
      io.postPipe(i) := io.prePipe(i)
      if(vldEn)
        io.postPipeVld.get(i) := io.prePipeVld.get(i)
    }
  }
}



// runMain Rs.GenPipe
//object GenPipe extends App {
//  ChiselStage.emitSystemVerilogFile(new GfPolyEval2, Array())
//}

//println(getVerilogString(new pipeline(new child_1, 3, 11), pretty))
