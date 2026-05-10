package core
import chisel3._
import chisel3.util._
class Eval4(inWidth: Int, outWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(4, UInt(inWidth.W)))
    val out = Output(Vec(7, UInt(outWidth.W)))
  })

  val even = io.in(0) +& io.in(2)
  val odd = io.in(1) +& io.in(3)
  val scaledEven = Cat(io.in(0), 0.U(2.W)) +& io.in(2)
  val scaledOdd = Cat(io.in(1), 0.U(2.W)) +& io.in(3)

  val high0 = io.in(2) +& Cat(io.in(3), 0.U(1.W))
  val high1 = io.in(1) +& Cat(high0, 0.U(1.W))
  val high2 = io.in(0) +& Cat(high1, 0.U(1.W))

  io.out(0) := ParaMath.mask(io.in(3), outWidth)
  io.out(1) := ParaMath.mask(high2, outWidth)
  io.out(2) := ParaMath.mask(even +& odd, outWidth)
  io.out(3) := ParaMath.fillMsb(even -& odd, outWidth)
  io.out(4) := ParaMath.mask(Cat(scaledEven, 0.U(1.W)) +& scaledOdd, outWidth)
  io.out(5) := ParaMath.fillMsb(Cat(scaledEven, 0.U(1.W)) -& scaledOdd, outWidth)
  io.out(6) := ParaMath.mask(io.in(0), outWidth)
}

/* class Evaluation16(
    aWidth: Int,
    bWidth: Int,
    aEvalWidth: Int,
    bEvalWidth: Int
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(16, UInt(aWidth.W)))
    val b = Input(Vec(16, UInt(bWidth.W)))
    val valid_out = Output(Bool())
    val A_eval = Output(Vec(28, UInt(aEvalWidth.W)))
    val B_eval = Output(Vec(28, UInt(bEvalWidth.W)))
  })

  for (seg <- 0 until 4) {
    val evalA = Module(new Eval4(aWidth, aEvalWidth))
    val evalB = Module(new Eval4(bWidth, bEvalWidth))

    for (i <- 0 until 4) {
      evalA.io.in(i) := io.a(seg * 4 + i)
      evalB.io.in(i) := io.b(seg * 4 + i)
    }

    for (pt <- 0 until 7) {
      io.A_eval(pt * 4 + seg) := evalA.io.out(pt)
      io.B_eval(pt * 4 + seg) := evalB.io.out(pt)
    }
  }

  io.valid_out := io.valid_in
} */