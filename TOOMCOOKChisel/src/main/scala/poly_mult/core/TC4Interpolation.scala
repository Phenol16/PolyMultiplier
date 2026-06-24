package poly_mult

import chisel3._
import chisel3.util._

class TC4InterpolationIO(val subLen: Int, val inW: Int, val outW: Int) extends Bundle {
  val valid_in = Input(Bool())
  val w = Input(Vec(7, Vec(subLen, UInt(inW.W))))
  val valid_out = Output(Bool())
  val c = Output(Vec(4 * subLen, UInt(outW.W)))
}

/**
  * Parameterized version of the original toomcook16 interpolation layer.
  *
  * The per-column r1/r2/r3/r4/r5 formulas and the negacyclic cWire assembly are
  * kept in the same order as interpolation.scala. The original MASK27/MASK24
  * and inverse constants are replaced by inW/outW/interpW-derived parameters.
  */
class TC4Interpolation(subLen: Int, inW: Int, interpW: Int, outW: Int) extends Module {
  require(subLen > 0)
  require(interpW >= inW)
  require(interpW >= outW)

  val io = IO(new TC4InterpolationIO(subLen, inW, outW))

  private def mod(value: UInt): UInt = TC4BitUtil.low(value, interpW)
  private def out(value: UInt): UInt = TC4BitUtil.low(value, outW)
  private def arShift(value: UInt, amount: Int): UInt =
    TC4BitUtil.low((mod(value).asSInt >> amount).asUInt, interpW)

  private val inv3 = TC4BitUtil.oddInverseModPow2(3, interpW).U(interpW.W)
  private val inv9 = TC4BitUtil.oddInverseModPow2(9, interpW).U(interpW.W)
  private val inv15 = TC4BitUtil.oddInverseModPow2(15, interpW).U(interpW.W)

  val w = Wire(Vec(7, Vec(subLen, UInt(interpW.W))))
  for (p <- 0 until 7; j <- 0 until subLen) {
    w(p)(j) := TC4BitUtil.low(io.w(p)(j), interpW)
  }

  val r1 = Wire(Vec(subLen, UInt(interpW.W)))
  val r2 = Wire(Vec(subLen, UInt(interpW.W)))
  val r3 = Wire(Vec(subLen, UInt(interpW.W)))
  val r4 = Wire(Vec(subLen, UInt(interpW.W)))
  val r5 = Wire(Vec(subLen, UInt(interpW.W)))

  for (j <- 0 until subLen) {
    val w0 = w(0)(j); val w1 = w(1)(j); val w2 = w(2)(j); val w3 = w(3)(j)
    val w4 = w(4)(j); val w5 = w(5)(j); val w6 = w(6)(j)

    val r1a = mod(w1 +& w4)
    val r5a = mod(w5 -& w4)
    val r3a = arShift(w3 -& w2, 1)
    val r4a = mod(w4 -& w0)
    val r4b = mod((r4a << 1) +& r5a -& (w6 << 7))
    val r2a = mod(w2 +& r3a)
    val r1b = mod(r1a -& (r2a << 6) -& r2a)
    val r2b = mod(r2a -& w6 -& w0)
    val r1c = mod(r1b +& r2b +& (r2b << 2) +& (r2b << 3) +& (r2b << 5))

    val r4d = mod(arShift(r4b -& (r2b << 3), 3) * inv3)
    val r5c = mod(arShift(r5a +& r1c, 1) * inv15)
    val r1e = mod(arShift(r1c +& (r3a << 4), 1) * inv9)

    val r2c = mod(r2b -& r4d)
    val r3b = mod(0.U(interpW.W) -& r1e -& r3a)
    val r5d = arShift(r1e -& r5c, 1)
    val r1f = mod(r1e -& r5d)

    r1(j) := r1f
    r2(j) := r2c
    r3(j) := r3b
    r4(j) := r4d
    r5(j) := r5d
  }

  val cWire = Wire(Vec(4 * subLen, UInt(outW.W)))
  for (j <- 0 until subLen) {
    val prev = if (j == 0) subLen - 1 else j - 1
    val base = 4 * j
    if (j == 0) {
      cWire(base + 0) := out(w(6)(j) -& r2(prev))
      cWire(base + 1) := out(r5(j) -& r1(prev))
      cWire(base + 2) := out(r4(j) -& w(0)(prev))
    } else {
      cWire(base + 0) := out(w(6)(j) +& r2(prev))
      cWire(base + 1) := out(r5(j) +& r1(prev))
      cWire(base + 2) := out(r4(j) +& w(0)(prev))
    }
    cWire(base + 3) := out(r3(j))
  }

  val validReg = RegNext(io.valid_in, false.B)
  val cReg = RegNext(cWire)

  io.valid_out := validReg
  io.c := cReg
}
