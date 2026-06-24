package core

import chisel3._

object ParamInterp4 {
  private def mod(value: UInt, width: Int): UInt = CoreBitUtil.mask(value, width)

  private def arithShiftRightMod(value: UInt, width: Int, amount: Int): UInt = {
    require(amount >= 0, "shift amount must be non-negative")
    val signed = CoreBitUtil.signExtend(value, width).asSInt
    CoreBitUtil.mask((signed >> amount).asUInt, width)
  }

  def productCoeffs(w: Seq[UInt], width: Int): Seq[UInt] = {
    require(w.length == 7, s"Toom-Cook-4 interpolation needs 7 points, got ${w.length}")
    require(width > 0, "interpolation width must be positive")

    val iw = w.map(x => mod(x, width))
    val inv3 = CoreModConst.inv3(width).U(width.W)
    val inv9 = CoreModConst.inv9(width).U(width.W)
    val inv15 = CoreModConst.inv15(width).U(width.W)

    val r1a = mod(iw(1) + iw(4), width)
    val r5a = mod(iw(5) - iw(4), width)
    val r3a = arithShiftRightMod(iw(3) - iw(2), width, 1)
    val r4a = mod(iw(4) - iw(0), width)

    val r4b = mod((r4a << 1) + r5a - (iw(6) << 7), width)
    val r2a = mod(iw(2) + r3a, width)
    val r1b = mod(r1a - (r2a << 6) - r2a, width)
    val r2b = mod(r2a - iw(6) - iw(0), width)

    val r1c = mod(r1b + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), width)
    val r4d = mod(arithShiftRightMod(r4b - (r2b << 3), width, 3) * inv3, width)
    val r1e = mod(arithShiftRightMod(r1c + (r3a << 4), width, 1) * inv9, width)
    val r5c = mod(arithShiftRightMod(r5a + r1c, width, 1) * inv15, width)

    val r2c = mod(r2b - r4d, width)
    val r3b = mod(0.U(width.W) - r3a - r1e, width)
    val r5d = arithShiftRightMod(r1e - r5c, width, 1)
    val r1f = mod(r1e - r5d, width)

    // Product coefficients in ascending order p0..p6.
    Seq(iw(6), r5d, r4d, r3b, r2c, r1f, iw(0))
  }

  def negacyclic4(w: Seq[UInt], width: Int): Vec[UInt] = {
    val p = productCoeffs(w, width)
    val out = Wire(Vec(4, UInt(width.W)))
    out(0) := mod(p(0) - p(4), width)
    out(1) := mod(p(1) - p(5), width)
    out(2) := mod(p(2) - p(6), width)
    out(3) := p(3)
    out
  }
}
