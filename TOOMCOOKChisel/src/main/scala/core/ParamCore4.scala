package core

import chisel3._

class ParamCore4(cfg: CoreConfig) extends Module {
  require(cfg.coreN == 4, s"ParamCore4 requires coreN=4, got ${cfg.coreN}")

  val io = IO(new ParamCoreIO(cfg))

  val core = Module(new ParamToomCook4(cfg.aInW, cfg.bInW, cfg.coreOutW))
  core.io.valid_in := io.valid_in
  core.io.a := io.a
  core.io.b := io.b
  io.valid_out := core.io.valid_out
  io.c := core.io.c
}
