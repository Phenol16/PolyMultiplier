package core

import chisel3._

class ParamCore64(cfg: CoreConfig) extends Module {
  require(cfg.coreN == 64, s"ParamCore64 requires coreN=64, got ${cfg.coreN}")

  val io = IO(new ParamCoreIO(cfg))

  // First-stage baseline: clear combinational schoolbook negacyclic hardware.
  // This is intentionally correct-before-fast; future work can replace it with
  // a pipelined hierarchy of Toom-Cook cores while preserving this IO contract.
  ParamSchoolbookBaseline.connect(io, cfg)
}
