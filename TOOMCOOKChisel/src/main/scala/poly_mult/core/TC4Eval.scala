package poly_mult

import chisel3._
import chisel3.util._

/**
  * Parameterized version of the original toomcook16 evaluation layer.
  *
  * The formulas and point order are unchanged; only the input width, output
  * width, number of coefficient lanes, and whether source coefficients are
  * already two's-complement evaluation values are parameters. Negative
  * evaluation points are explicitly represented as outW-bit two's-complement.
  */
class TC4Eval(inW: Int, outW: Int, lanes: Int, signedInputs: Boolean = false) extends Module {
  require(lanes > 0)
  require(outW >= inW + 3)

  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val x = Input(Vec(4, Vec(lanes, UInt(inW.W))))
    val valid_out = Output(Bool())
    val y = Output(Vec(7, Vec(lanes, UInt(outW.W))))
  })

  private def extendInput(value: UInt): UInt = {
    if (signedInputs) TC4BitUtil.signExtendOrTruncate(value, outW)
    else TC4BitUtil.low(value, outW)
  }
  private def fit(value: UInt): UInt = TC4BitUtil.low(value, outW)

  for (j <- 0 until lanes) {
    val x0 = extendInput(io.x(0)(j))
    val x1 = extendInput(io.x(1)(j))
    val x2 = extendInput(io.x(2)(j))
    val x3 = extendInput(io.x(3)(j))

    val even = fit(x0 +& x2)
    val odd = fit(x1 +& x3)
    val scaledEven = fit((x0 << 2) +& x2)
    val scaledOdd = fit((x1 << 2) +& x3)

    val h0 = fit(x2 +& (x3 << 1))
    val h1 = fit(x1 +& (h0 << 1))
    val h2 = fit(x0 +& (h1 << 1))

    io.y(0)(j) := x3                                      // infinity
    io.y(1)(j) := h2                                      // 2
    io.y(2)(j) := fit(even +& odd)                        // 1
    io.y(3)(j) := fit(even -& odd)                        // -1
    io.y(4)(j) := fit((scaledEven << 1) +& scaledOdd)     // 1/2 scaled
    io.y(5)(j) := fit((scaledEven << 1) -& scaledOdd)     // -1/2 scaled
    io.y(6)(j) := x0                                      // 0
  }

  io.valid_out := io.valid_in
}
