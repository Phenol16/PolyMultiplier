package poly_mult

import chisel3._
import chisel3.util._

class Core16 extends Module {
  private val A_EVAL_W = PolyMulConfig.A_EVAL_W
  private val B_EVAL_W = PolyMulConfig.B_EVAL_W
  private val CORE_OUT_W = PolyMulConfig.CORE_OUT_W

  val io = IO(new Bundle {
    val valid_in  = Input(Bool())
    val avec      = Input(Vec(16, UInt(A_EVAL_W.W)))
    val bvec      = Input(Vec(16, UInt(B_EVAL_W.W)))
    val valid_out = Output(Bool())
    val cOut      = Output(Vec(16, UInt(CORE_OUT_W.W)))
  })

  val ae = Wire(Vec(7 * 4, UInt(A_EVAL_W.W)))
  val be = Wire(Vec(7 * 4, UInt(B_EVAL_W.W)))

  for (seg <- 0 until 4) {
    val evalA = Module(new EvalLayer(A_EVAL_W, A_EVAL_W))
    val evalB = Module(new EvalLayer(B_EVAL_W, B_EVAL_W))

    evalA.io.r(0) := io.avec(seg * 4 + 0)
    evalA.io.r(1) := io.avec(seg * 4 + 1)
    evalA.io.r(2) := io.avec(seg * 4 + 2)
    evalA.io.r(3) := io.avec(seg * 4 + 3)

    evalB.io.r(0) := io.bvec(seg * 4 + 0)
    evalB.io.r(1) := io.bvec(seg * 4 + 1)
    evalB.io.r(2) := io.bvec(seg * 4 + 2)
    evalB.io.r(3) := io.bvec(seg * 4 + 3)

    for (pt <- 0 until 7) {
      ae(pt * 4 + seg) := evalA.io.out(pt)
      be(pt * 4 + seg) := evalB.io.out(pt)
    }
  }

  val wProd = Wire(Vec(7 * 4, UInt(CORE_OUT_W.W)))

  for (pt <- 0 until 7) {
    val prod = Module(new Product4)

    for (k <- 0 until 4) {
      prod.io.a4(k) := ae(pt * 4 + k)
      prod.io.b4(k) := be(pt * 4 + k)
    }

    for (k <- 0 until 4) {
      wProd(pt * 4 + k) := prod.io.out(k)
    }
  }

  val regW     = RegEnable(wProd, io.valid_in)
  val regValid = RegNext(io.valid_in, false.B)

  val interp = Module(new InterPlayer(stride = 4, pidx = 0, inW = 36, outW = 36))
  interp.io.wIn := regW

  io.valid_out := regValid
  io.cOut      := interp.io.cOut
}
