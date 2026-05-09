package core

import chisel3._

class ParamCore16(cfg: CoreConfig) extends Module {
  require(cfg.coreN == 16, s"ParamCore16 requires coreN=16, got ${cfg.coreN}")

  val io = IO(new ParamCoreIO(cfg))

  // First-stage baseline: parameterized signed schoolbook negacyclic hardware.
  // The module boundary is intentionally separate so this body can later be
  // replaced by a parameterized Toom-Cook-16 datapath without changing tests.
  ParamSchoolbookBaseline.connect(io, cfg)
}
