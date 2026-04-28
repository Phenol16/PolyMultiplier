package poly_mult

import chisel3._
import chisel3.util._

// EvalVec16ParallelLegacy：旧版全并行三层 TC4 求值，
// 当前顶层默认不使用，除非显式实例化。
// 输入布局：in[64*l + 16*k + 4*j + i]
class EvalVec16ParallelLegacy(memW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Input(Vec(1024, UInt(memW.W)))
    val pt0 = Input(UInt(3.W))
    val pt1 = Input(UInt(3.W))
    val pt2 = Input(UInt(3.W))
    val out = Output(Vec(16, UInt(outW.W)))
  })

  for (l <- 0 until 16) {
    val lv2 = Wire(Vec(4, UInt(outW.W)))

    for (k <- 0 until 4) {
      val lv1 = Wire(Vec(4, UInt(outW.W)))

      for (j <- 0 until 4) {
        val eval0 = Module(new EvalPoint(memW, outW))
        eval0.io.r(0) := io.in(64 * l + 16 * k + 4 * j + 0)
        eval0.io.r(1) := io.in(64 * l + 16 * k + 4 * j + 1)
        eval0.io.r(2) := io.in(64 * l + 16 * k + 4 * j + 2)
        eval0.io.r(3) := io.in(64 * l + 16 * k + 4 * j + 3)
        eval0.io.pt   := io.pt0
        lv1(j)        := eval0.io.out
      }

      val eval1 = Module(new EvalPoint(outW, outW))
      eval1.io.r := lv1
      eval1.io.pt := io.pt1
      lv2(k) := eval1.io.out
    }

    val eval2 = Module(new EvalPoint(outW, outW))
    eval2.io.r := lv2
    eval2.io.pt := io.pt2
    io.out(l) := eval2.io.out
  }
}
