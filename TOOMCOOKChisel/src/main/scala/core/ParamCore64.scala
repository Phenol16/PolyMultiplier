package core

import chisel3._

class ParamCore64(cfg: CoreConfig) extends Module {
  require(cfg.coreN == 64, s"ParamCore64 requires coreN=64, got ${cfg.coreN}")

  val io = IO(new ParamCoreIO(cfg))

  val core = Module(new ParamToomCook64Block(cfg.aInW, cfg.bInW, cfg.coreOutW))
  core.io.valid_in := io.valid_in
  core.io.a := io.a
  core.io.b := io.b
  io.valid_out := core.io.valid_out
  io.c := core.io.c
}
