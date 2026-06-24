package core

import chisel3._

class ParamCoreIO(cfg: CoreConfig) extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(cfg.coreN, UInt(cfg.aInW.W)))
  val b = Input(Vec(cfg.coreN, UInt(cfg.bInW.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(cfg.coreN, UInt(cfg.coreOutW.W)))
}
