package poly_mult

import chisel3._
import chisel3.util._

class EvalMath(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r   = Input(Vec(4, UInt(inW.W)))
    val out = Output(Vec(7, UInt(outW.W)))
  })

  val even = io.r(0) +& io.r(2)
  val odd  = io.r(1) +& io.r(3)

  val scaledEven = Cat(io.r(0), 0.U(2.W)) +& io.r(2)
  val scaledOdd  = Cat(io.r(1), 0.U(2.W)) +& io.r(3)

  val high0 = io.r(2) +& Cat(io.r(3), 0.U(1.W))
  val high1 = io.r(1) +& Cat(high0, 0.U(1.W))
  val high2 = io.r(0) +& Cat(high1, 0.U(1.W))

  io.out(0) := BitUtil.mask(io.r(3), outW)
  io.out(1) := BitUtil.mask(high2, outW)
  io.out(2) := BitUtil.mask(even +& odd, outW)
  io.out(3) := BitUtil.fillMsb(even -& odd, outW)
  io.out(4) := BitUtil.mask(Cat(scaledEven, 0.U(1.W)) +& scaledOdd, outW)
  io.out(5) := BitUtil.fillMsb(Cat(scaledEven, 0.U(1.W)) -& scaledOdd, outW)
  io.out(6) := BitUtil.mask(io.r(0), outW)
}
