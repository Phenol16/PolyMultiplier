package core

import chisel3.util.log2Ceil

case class CoreConfig(
    coreN: Int,
    aInW: Int,
    bInW: Int,
    aEvalW: Int,
    bEvalW: Int,
    coreOutW: Int,
    topOutW: Int = 24,
    radix: Int = 4,
    numPoints: Int = 7
)

object CoreConfig {
  val supportedCoreNs: Seq[Int] = Seq(4, 16, 64)
  val supportedAInWs: Seq[Int] = Seq(24, 28, 32, 36)
  val supportedBInWs: Seq[Int] = Seq(8, 10, 12, 14, 16)

  def derive(coreN: Int, aInW: Int, bInW: Int): CoreConfig = {
    require(supportedCoreNs.contains(coreN), s"unsupported coreN=$coreN, supported=${supportedCoreNs.mkString(",")}")
    require(supportedAInWs.contains(aInW), s"unsupported aInW=$aInW, supported=${supportedAInWs.mkString(",")}")
    require(supportedBInWs.contains(bInW), s"unsupported bInW=$bInW, supported=${supportedBInWs.mkString(",")}")

    // Toom-Cook evaluation at points including +/-1, +/-2 and infinity can
    // temporarily add several shifted coefficients.  Keep a conservative four
    // guard bits over the raw inputs so future ParamEval4/ParamInterp4 modules
    // can be dropped in without scattering width constants through the core.
    val evalGrowth = 4
    val aEvalW = aInW + evalGrowth
    val bEvalW = bInW + evalGrowth

    // A negacyclic schoolbook output coefficient accumulates coreN signed
    // products.  A product needs aInW + bInW bits modulo two's complement; the
    // accumulation needs log2Ceil(coreN) carry bits.  Four extra guard bits keep
    // this baseline and later interpolation experiments safely aligned with the
    // reference model's final modulo 2^coreOutW truncation.
    val coreOutW = aInW + bInW + log2Ceil(coreN) + 4

    CoreConfig(
      coreN = coreN,
      aInW = aInW,
      bInW = bInW,
      aEvalW = aEvalW,
      bEvalW = bEvalW,
      coreOutW = coreOutW
    )
  }
}
