package poly_mult

import chisel3._
import chisel3.util._

// EvalPoint：4个输入按 pt 选择一个求值点，纯组合硬件模块
class EvalPoint(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r   = Input(Vec(4, UInt(inW.W)))
    val pt  = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  val points = EvalMath.eval4Points(io.r(0), io.r(1), io.r(2), io.r(3), outW)
  io.out := MuxLookup(io.pt, 0.U(outW.W))((0 until 7).map(i => i.U -> points(i)))
}
