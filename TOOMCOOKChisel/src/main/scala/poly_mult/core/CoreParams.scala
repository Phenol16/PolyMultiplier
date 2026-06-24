package poly_mult

import chisel3._
import chisel3.util._

/**
  * Parameter bundle and bit helpers for the parameterized Toom-Cook-4 core.
  *
  * This file centralizes the widths that were hard-coded in the original
  * toomcook16 path (24/8 inputs, 30/16 evaluation values, 27/24 interpolation
  * masks) so the same evaluation -> 7 sub-cores -> interpolation structure can
  * be reused at several 1024-level stages.
  */
case class CoreParams(
    n: Int,
    aInW: Int,
    bInW: Int,
    aEvalW: Int,
    bEvalW: Int,
    mulOutW: Int,
    interpW: Int,
    outW: Int
) {
  require(Set(4, 16, 64).contains(n), s"n must be 4, 16, or 64, got $n")
  require(aInW > 0 && bInW > 0, "input widths must be positive")
  require(aEvalW >= aInW, "aEvalW must cover aInW")
  require(bEvalW >= bInW, "bEvalW must cover bInW")
  require(mulOutW >= outW, "mulOutW must cover outW")
  require(interpW >= outW, "interpW must cover outW")
  require(interpW >= mulOutW, "interpW must cover mulOutW")
  require(aEvalW >= aInW + 3, "aEvalW should cover TC4 evaluation growth")
  require(bEvalW >= bInW + 3, "bEvalW should cover TC4 evaluation growth")

  val subLen: Int = n / 4
}

/** Width defaults inspired by the ToomCook1024 layering style: explicit input,
  * evaluation, core product/interpolation, and final output widths. Callers may
  * still pass a fully explicit CoreParams instead of using this policy.
  */
object CoreWidthPolicy {
  def defaultFor(n: Int, aInW: Int, bInW: Int, targetOutW: Int): CoreParams = {
    val evalGuard = 6
    val productGrowth = log2Ceil(n) + 12
    val workW = math.max(targetOutW + productGrowth, aInW + bInW + productGrowth)
    CoreParams(
      n = n,
      aInW = aInW,
      bInW = bInW,
      aEvalW = aInW + evalGuard,
      bEvalW = bInW + evalGuard,
      mulOutW = workW,
      interpW = workW,
      outW = targetOutW
    )
  }
}

object TC4BitUtil {
  def mask(width: Int): BigInt = (BigInt(1) << width) - 1

  /** Elaboration-time inverse of an odd integer modulo 2^width. */
  def oddInverseModPow2(odd: BigInt, width: Int): BigInt = {
    require(width > 0, "width must be positive")
    require((odd & 1) == 1, s"oddInverseModPow2 requires odd input, got $odd")
    odd.modInverse(BigInt(1) << width) & mask(width)
  }

  def maskConst(width: Int): UInt = mask(width).U(width.W)

  /** Keep the low targetWidth bits, i.e. arithmetic modulo 2^targetWidth. */
  def low(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth == targetWidth) value
    else if (value.getWidth > targetWidth) value(targetWidth - 1, 0)
    else Cat(0.U((targetWidth - value.getWidth).W), value)
  }

  /** Explicit two's-complement sign extension/truncation used by evaluation. */
  def signExtendOrTruncate(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth == targetWidth) value
    else if (value.getWidth > targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
  }

  def addMod(a: UInt, b: UInt, width: Int): UInt = low(a +& b, width)
  def subMod(a: UInt, b: UInt, width: Int): UInt = low(a -& b, width)
}
