package poly_mult

import chisel3._
import chisel3.util._

object EvalMath {
  def eval4Points(r0: UInt, r1: UInt, r2: UInt, r3: UInt, outW: Int): Vec[UInt] = {
    val out = Wire(Vec(7, UInt(outW.W)))

    val even = r0 +& r2
    val odd  = r1 +& r3

    val scaledEven = Cat(r0, 0.U(2.W)) +& r2
    val scaledOdd  = Cat(r1, 0.U(2.W)) +& r3

    val high0 = r2 +& Cat(r3, 0.U(1.W))
    val high1 = r1 +& Cat(high0, 0.U(1.W))
    val high2 = r0 +& Cat(high1, 0.U(1.W))

    out(0) := BitUtil.mask(r3, outW)
    out(1) := BitUtil.mask(high2, outW)
    out(2) := BitUtil.mask(even +& odd, outW)
    out(3) := BitUtil.fillMsb(even -& odd, outW)
    out(4) := BitUtil.mask(Cat(scaledEven, 0.U(1.W)) +& scaledOdd, outW)
    out(5) := BitUtil.fillMsb(Cat(scaledEven, 0.U(1.W)) -& scaledOdd, outW)
    out(6) := BitUtil.mask(r0, outW)

    out
  }
}
