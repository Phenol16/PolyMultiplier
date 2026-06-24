package core

import chisel3._

object ParamEval4 {
  def apply(coeffs: Seq[UInt], inW: Int, evalW: Int): Vec[UInt] = {
    require(coeffs.length == 4, s"Toom-Cook-4 evaluation needs 4 coefficients, got ${coeffs.length}")
    require(evalW >= inW + 4, s"evalW=$evalW must leave at least 4 guard bits over inW=$inW")

    val x = coeffs.map(c => CoreBitUtil.signExtend(c, evalW).asSInt)
    val out = Wire(Vec(7, UInt(evalW.W)))

    // Evaluation order follows the existing ToomCook16 kernel:
    // infinity, 2, 1, -1, 1/2 scaled by 8, -1/2 scaled by 8, 0.
    val pInf = x(3)
    val p2 = x(0) + (x(1) << 1) + (x(2) << 2) + (x(3) << 3)
    val p1 = x(0) + x(1) + x(2) + x(3)
    val pm1 = x(0) - x(1) + x(2) - x(3)
    val ph = (x(0) << 3) + (x(1) << 2) + (x(2) << 1) + x(3)
    val pmh = (x(0) << 3) - (x(1) << 2) + (x(2) << 1) - x(3)
    val p0 = x(0)

    Seq(pInf, p2, p1, pm1, ph, pmh, p0).zipWithIndex.foreach { case (value, idx) =>
      out(idx) := CoreBitUtil.mask(value.asUInt, evalW)
    }
    out
  }
}
