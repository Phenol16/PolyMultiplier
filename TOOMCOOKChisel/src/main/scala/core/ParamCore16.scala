package core

import chisel3._

class ParamCore16(cfg: CoreConfig) extends Module {
  require(cfg.coreN == 16, s"ParamCore16 requires coreN=16, got ${cfg.coreN}")

  val io = IO(new ParamCoreIO(cfg))

  val core = Module(new ParamToomCook16Block(cfg.aInW, cfg.bInW, cfg.coreOutW))
  core.io.valid_in := io.valid_in
  core.io.a := io.a
  core.io.b := io.b
  io.valid_out := core.io.valid_out
  io.c := core.io.c
}
