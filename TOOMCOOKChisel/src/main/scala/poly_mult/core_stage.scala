package poly_mult

import chisel3._
import chisel3.util._

class CoreStageIO extends Bundle {
  val in = Flipped(Decoupled(new CoreInRecord))
  val out = Decoupled(new W2Record)
}

// CoreStage：一次只处理一组，等待 Core16TC43 完成再接下一组
class CoreStage extends Module {
  val io = IO(new CoreStageIO)

  val core = Module(new Core16TC43)
  val busy = RegInit(false.B)
  val outValid = RegInit(false.B)
  val outReg = Reg(new W2Record)

  core.io.valid_in := io.in.fire
  core.io.a := io.in.bits.aEval
  core.io.b := io.in.bits.bEval

  io.in.ready := (!busy) && (!outValid)

  when(io.in.fire) {
    outReg.seg := io.in.bits.seg
    outReg.pt0 := io.in.bits.pt0
    outReg.pt1 := io.in.bits.pt1
    outReg.pt2 := io.in.bits.pt2
    busy := true.B
  }

  when(core.io.valid_out) {
    for (i <- 0 until 16) {
      outReg.data(i) := core.io.c(i)
    }
    busy := false.B
    outValid := true.B
  }

  io.out.valid := outValid
  io.out.bits := outReg

  when(io.out.fire) {
    outValid := false.B
  }
}
