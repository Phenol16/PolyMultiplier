package poly_mult

import chisel3._
import chisel3.util._

class InterpStageIO extends Bundle {
  val in = Flipped(Decoupled(new W2Record))
  val out = Valid(Vec(1024, UInt(24.W)))
}

// InterpStage：三级 group buffer
class InterpStage extends Module {
  val io = IO(new InterpStageIO)

  val interp16 = Module(new InterpLayer(16, 27, 27))
  val interp64 = Module(new InterpLayer(64, 27, 27))
  val interp256 = Module(new InterpLayer(256, 27, 27))

  val w2Group = Reg(Vec(7 * 16, UInt(27.W)))
  val w1Group = Reg(Vec(7 * 64, UInt(27.W)))
  val w0Group = Reg(Vec(7 * 256, UInt(27.W)))

  val outReg = Reg(Vec(1024, UInt(24.W)))
  val outValidReg = RegInit(false.B)

  val pt0Cnt = RegInit(0.U(3.W))
  val pt1Cnt = RegInit(0.U(3.W))
  val pt2Cnt = RegInit(0.U(3.W))

  val sCollect :: sStart16 :: sWait16 :: sStart64 :: sWait64 :: sStart256 :: sWait256 :: Nil = Enum(7)
  val state = RegInit(sCollect)

  interp16.io.start := false.B
  interp64.io.start := false.B
  interp256.io.start := false.B

  interp16.io.in := w2Group
  interp64.io.in := w1Group
  interp256.io.in := w0Group

  io.in.ready := (state === sCollect)

  io.out.valid := outValidReg
  io.out.bits := outReg

  when(outValidReg) {
    outValidReg := false.B
  }

  switch(state) {
    is(sCollect) {
      when(io.in.fire) {
        val base = pt2Cnt << 4
        for (i <- 0 until 16) {
          w2Group(base + i.U) := io.in.bits.data(i)
        }

        when(pt2Cnt === 6.U) {
          state := sStart16
        }.otherwise {
          pt2Cnt := pt2Cnt + 1.U
        }
      }
    }

    is(sStart16) {
      interp16.io.start := true.B
      state := sWait16
    }

    is(sWait16) {
      when(interp16.io.done) {
        val base = pt1Cnt << 6
        for (i <- 0 until 64) {
          w1Group(base + i.U) := interp16.io.out(i)
        }
        pt2Cnt := 0.U
        when(pt1Cnt === 6.U) {
          state := sStart64
        }.otherwise {
          pt1Cnt := pt1Cnt + 1.U
          state := sCollect
        }
      }
    }

    is(sStart64) {
      interp64.io.start := true.B
      state := sWait64
    }

    is(sWait64) {
      when(interp64.io.done) {
        val base = pt0Cnt << 8
        for (i <- 0 until 256) {
          w0Group(base + i.U) := interp64.io.out(i)
        }
        pt1Cnt := 0.U
        when(pt0Cnt === 6.U) {
          state := sStart256
        }.otherwise {
          pt0Cnt := pt0Cnt + 1.U
          state := sCollect
        }
      }
    }

    is(sStart256) {
      interp256.io.start := true.B
      state := sWait256
    }

    is(sWait256) {
      when(interp256.io.done) {
        for (i <- 0 until 1024) {
          outReg(i) := interp256.io.out(i)(23, 0)
        }
        outValidReg := true.B
        pt0Cnt := 0.U
        pt1Cnt := 0.U
        pt2Cnt := 0.U
        state := sCollect
      }
    }
  }
}