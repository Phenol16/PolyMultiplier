package core

import chisel3._
import chisel3.util.log2Ceil

object ParamSchoolbookBaseline {
  private def signedCoeff(value: UInt, width: Int): SInt =
    CoreBitUtil.signExtend(value, width + 1).asSInt

  private def resizeSigned(value: SInt, width: Int): SInt =
    CoreBitUtil.signExtend(value.asUInt, width).asSInt

  def connect(io: ParamCoreIO, cfg: CoreConfig): Unit = {
    val accW = cfg.aInW + cfg.bInW + log2Ceil(cfg.coreN) + 4
    require(accW <= cfg.coreOutW, s"accW=$accW must fit coreOutW=${cfg.coreOutW}")

    val aSigned = Seq.tabulate(cfg.coreN)(i => signedCoeff(io.a(i), cfg.aInW))
    val bSigned = Seq.tabulate(cfg.coreN)(i => signedCoeff(io.b(i), cfg.bInW))

    val outComb = Wire(Vec(cfg.coreN, UInt(cfg.coreOutW.W)))

    for (k <- 0 until cfg.coreN) {
      val terms = for {
        i <- 0 until cfg.coreN
        j <- 0 until cfg.coreN
        rawIdx = i + j
        if rawIdx == k || rawIdx - cfg.coreN == k
      } yield {
        val product = aSigned(i) * bSigned(j)
        val resized = resizeSigned(product, accW)
        if (i + j < cfg.coreN) resized else -resized
      }

      val sum = terms.reduceOption(_ +& _).getOrElse(0.S(accW.W))
      outComb(k) := CoreBitUtil.mask(sum.asUInt, cfg.coreOutW)
    }

    // The baseline is combinational for coefficients and has one registered
    // valid/result boundary.  This keeps a fixed one-cycle latency while leaving
    // room to replace the body with pipelined Toom-Cook hardware later.
    io.valid_out := RegNext(io.valid_in, false.B)
    io.c := RegNext(outComb)
  }
}
