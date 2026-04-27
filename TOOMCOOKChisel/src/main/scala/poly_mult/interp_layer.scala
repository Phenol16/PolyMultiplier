package poly_mult

import chisel3._
import chisel3.util._

class InterpLayerIO(val stride: Int, val inW: Int, val outW: Int)
    extends Bundle {
  val start = Input(Bool())
  val in = Input(Vec(7 * stride, UInt(inW.W)))
  val done = Output(Bool())
  val out = Output(Vec(4 * stride, UInt(outW.W)))
}

class InterpLayer(val stride: Int, val inW: Int, val outW: Int)
    extends Module {
  val io = IO(new InterpLayerIO(stride, inW, outW))

  val sIdle :: sBusy :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val cnt = RegInit(0.U(3.W))
  val inReg = Reg(Vec(7 * stride, UInt(inW.W)))
  val outReg = Reg(Vec(4 * stride, UInt(outW.W)))

  io.done := false.B
  io.out := outReg

  switch(state) {
    is(sIdle) {
      when(io.start) {
        for (i <- 0 until 7 * stride) {
          inReg(i) := io.in(i)
        }
        cnt := 0.U
        state := sBusy
      }
    }
    is(sBusy) {
      cnt := cnt + 1.U
      when(cnt === 3.U) {
        for (i <- 0 until 4 * stride) {
          outReg(i) := inReg(i)(outW - 1, 0)
        }
        state := sDone
      }
    }
    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}