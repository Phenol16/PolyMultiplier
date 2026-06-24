package core

import chisel3.stage.ChiselStage

object GenerateCoreSweep extends App {
  for {
    coreN <- CoreConfig.supportedCoreNs
    aW <- CoreConfig.supportedAInWs
    bW <- CoreConfig.supportedBInWs
  } {
    val cfg = CoreConfig.derive(coreN = coreN, aInW = aW, bInW = bW)
    val dir = s"generated/core/core${coreN}_a${aW}_b${bW}"

    println(
      s"[GenerateCoreSweep] generating coreN=${cfg.coreN}, aInW=${cfg.aInW}, bInW=${cfg.bInW}, " +
        s"aEvalW=${cfg.aEvalW}, bEvalW=${cfg.bEvalW}, coreOutW=${cfg.coreOutW}, dir=$dir"
    )

    (new ChiselStage).emitVerilog(
      new ParamCore(cfg),
      Array("--target-dir", dir)
    )
  }
}
