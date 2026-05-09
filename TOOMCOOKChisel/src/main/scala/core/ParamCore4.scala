package core

import chisel3._

class ParamCore4(cfg: CoreConfig) extends Module {
  require(cfg.coreN == 4, s"ParamCore4 requires coreN=4, got ${cfg.coreN}")

  val io = IO(new ParamCoreIO(cfg))

  // First-stage baseline: parameterized signed schoolbook negacyclic hardware
  // for a(x) * b(x) mod (x^4 + 1), modulo 2^coreOutW per coefficient.
  ParamSchoolbookBaseline.connect(io, cfg)
}
