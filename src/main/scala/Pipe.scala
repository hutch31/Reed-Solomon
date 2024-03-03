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

class GfPolyEval2 extends Module with GfParams {
  val io = IO(new Bundle {
    val errLocatorIf = Input(new ErrLocatorBundle)
    val inSymb = Input(Valid(UInt(symbWidth.W)))
    val evalValue = Output(Valid(UInt(symbWidth.W)))
  })

  class SymbXorVld extends Bundle {
    val xor = UInt(symbWidth.W)
    val symb = UInt(symbWidth.W)
    //val valid = Bool()
  }

  val gfMultIntrm = Wire(Vec(tLen, UInt(symbWidth.W)))

  val prePipe = Wire(Vec(tLen, new SymbXorVld))
  val postPipe = Wire(Vec(tLen-1, new SymbXorVld))
  val prePipeVld = Wire(Vec(tLen, Bool()))
  val postPipeVld = Wire(Vec(tLen-1, Bool()))

  for(i <- 0 until tLen) {
    if(i == 0) {
      gfMultIntrm(i) := gfMult(1, io.inSymb.bits)
      prePipeVld(i) := io.inSymb.valid
      prePipe(i).symb := io.inSymb.bits
    }else {
      gfMultIntrm(i) := gfMult(postPipe(i-1).xor, postPipe(i-1).symb)
      prePipeVld(i) := postPipeVld(i-1)
      prePipe(i).symb := postPipe(i-1).symb
    }
    prePipe(i).xor := gfMultIntrm(i) ^ io.errLocatorIf.errLocator(i)
  }

  /////////////////////////////////////////
  // Pipeline the comb logic if requred
  /////////////////////////////////////////

  val pipe = Module(new pipeline(new SymbXorVld, 0, tLen, true))
  pipe.io.prePipe := prePipe
  pipe.io.prePipeVld.get := prePipeVld
  postPipe := pipe.io.postPipe
  postPipeVld := pipe.io.postPipeVld.get

  // Add registers
  val prioEnvOut = Wire(UInt(log2Ceil(tLen).W))

  prioEnvOut := 0.U
  for(i <- (0 until tLen)) {
    when(io.errLocatorIf.errLocatorSel(i) === 1) {
      prioEnvOut := i.U
    }
  }

  io.evalValue.valid := prePipeVld(prioEnvOut)
  io.evalValue.bits := prePipe(prioEnvOut).xor
  

}

// runMain Rs.GenPipe
object GenPipe extends App {
  ChiselStage.emitSystemVerilogFile(new GfPolyEval2, Array())
}

//println(getVerilogString(new pipeline(new child_1, 3, 11), pretty))
