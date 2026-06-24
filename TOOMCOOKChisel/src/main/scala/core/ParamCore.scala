package core

import chisel3._

class ParamCore(cfg: CoreConfig) extends Module {
  override def desiredName: String = s"ParamCore${cfg.coreN}_A${cfg.aInW}_B${cfg.bInW}"

  val io = IO(new ParamCoreIO(cfg))

  val impl = cfg.coreN match {
    case 4 => Module(new ParamCore4(cfg))
    case 16 => Module(new ParamCore16(cfg))
    case 64 => Module(new ParamCore64(cfg))
    case other => throw new IllegalArgumentException(s"unsupported coreN=$other")
  }

  impl.io.valid_in := io.valid_in
  impl.io.a := io.a
  impl.io.b := io.b
  io.valid_out := impl.io.valid_out
  io.c := impl.io.c
}
