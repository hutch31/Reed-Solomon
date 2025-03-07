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
import circt.stage.ChiselStage
import chisel3.util._

class RsDecoder(c: Config) extends Module {
  val io = IO(new Bundle {
    val sAxisIf = Input(Valid(new axisIf(c.BUS_WIDTH, c.SYMB_WIDTH)))
    val errPosIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val errValIf = Output(Valid(new vecFfsIf(c.T_LEN, c.SYMB_WIDTH)))
    val chienErrDetect = Output(Bool())
    val msgCorrupted = Output(Bool())
    val syndValid = Output(Bool())
    val coreClock = Input(Clock())
    val coreRst = Input(Bool())
  })

  val rsSynd = Module(new RsSynd(c))
  rsSynd.io.sAxisIf <> io.sAxisIf

  // rsBm/rsChien/rsForney are connected to the coreClock/coreRst
  val rsBm = Module(new RsBmWrap(c))
  val rsChien = Module(new RsChien(c))
  val rsForney = Module(new RsForney(c))
  rsBm.clock := io.coreClock
  rsBm.reset := io.coreRst
  rsChien.clock := io.coreClock
  rsChien.reset := io.coreRst
  rsForney.clock := io.coreClock
  rsForney.reset := io.coreRst

  // If there are two clock domains then use toggle synchronizer
  // to sync syndIf.valid signal
  if(c.decoderSingleClock) {
    rsBm.io.syndIf <> rsSynd.io.syndIf
    rsForney.io.syndIf.bits := rsSynd.io.syndIf.bits
    rsForney.io.syndIf.valid := rsSynd.io.syndIf.valid
  } else {    
    val toggleSyndSyncr = Module(new ToggleSyncr())
    toggleSyndSyncr.clock := clock
    toggleSyndSyncr.reset := reset
    toggleSyndSyncr.io.in    := rsSynd.io.syndIf.valid
    toggleSyndSyncr.io.outClk := io.coreClock
    toggleSyndSyncr.io.outRst := io.coreRst

    rsBm.io.syndIf.bits := rsSynd.io.syndIf.bits
    rsBm.io.syndIf.valid := toggleSyndSyncr.io.out
    rsForney.io.syndIf.bits := rsSynd.io.syndIf.bits
    rsForney.io.syndIf.valid := toggleSyndSyncr.io.out
  }

  // Connect rsBm/rsChien/rsForney interfaces
  rsChien.io.errLocIf <> rsBm.io.errLocIf
  rsForney.io.errPosIf <> rsChien.io.errPosIf

  io.chienErrDetect <> rsChien.io.chienErrDetect
  if(c.decoderSingleClock){
    io.errPosIf <> rsChien.io.errPosIf
    io.errValIf <> rsForney.io.errValIf
  } else {
    val togglePosSyncr = Module(new ToggleSyncr())
    togglePosSyncr.clock := io.coreClock
    togglePosSyncr.reset := io.coreRst
    togglePosSyncr.io.in    := rsChien.io.errPosIf.valid
    togglePosSyncr.io.outClk := clock
    togglePosSyncr.io.outRst := reset

    io.errPosIf.bits := rsChien.io.errPosIf.bits
    io.errPosIf.valid := togglePosSyncr.io.out

    val toggleValSyncr = Module(new ToggleSyncr())
    toggleValSyncr.clock := io.coreClock
    toggleValSyncr.reset := io.coreRst
    toggleValSyncr.io.in    := rsForney.io.errValIf.valid
    toggleValSyncr.io.outClk := clock
    toggleValSyncr.io.outRst := reset

    io.errValIf.bits := rsForney.io.errValIf.bits
    io.errValIf.valid := toggleValSyncr.io.out
  }

  // if the syndrome is not zero then block corrupted
  io.msgCorrupted := rsSynd.io.syndIf.bits.reduce(_|_).orR
  io.syndValid := rsSynd.io.syndIf.valid
}

// runMain Rs.GenRsDecoder
//object GenRsDecoder extends App {
//  val config = JsonReader.readConfig("/home/egorman44/chisel-lib/rs.json")
//  ChiselStage.emitSystemVerilogFile(new RsDecoder(config), Array())
//}
