package poly_mult

import chisel3._
import chisel3.util._

// EvalLayer：4个输入 -> 7个 Toom-Cook 求值点，纯组合硬件模块
class EvalLayer(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r   = Input(Vec(4, UInt(inW.W)))
    val out = Output(Vec(7, UInt(outW.W)))
  })

  io.out := EvalMath.eval4Points(io.r(0), io.r(1), io.r(2), io.r(3), outW)
}
