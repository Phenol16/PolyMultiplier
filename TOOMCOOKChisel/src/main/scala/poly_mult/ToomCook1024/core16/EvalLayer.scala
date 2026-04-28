package poly_mult

import chisel3._
import chisel3.util._

// EvalLayer：4个输入 -> 7个 Toom-Cook 求值点，纯组合硬件模块
class EvalLayer(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r   = Input(Vec(4, UInt(inW.W)))
    val out = Output(Vec(7, UInt(outW.W)))
  })

  val evalMath = Module(new EvalMath(inW, outW))
  evalMath.io.r := io.r
  io.out := evalMath.io.out
}
