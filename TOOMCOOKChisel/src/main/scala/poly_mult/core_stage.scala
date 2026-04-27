package poly_mult

import chisel3._
import chisel3.util._

class CoreStageIO extends Bundle {
  val in = Flipped(Decoupled(new CoreInRecord))
  val out = Decoupled(new W2Record)
}


class CoreStage extends Module {
  val io = IO(new CoreStageIO)

  val core = Module(new Core16)
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

class Core16IO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(16, UInt(30.W)))
  val b = Input(Vec(16, UInt(16.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(16, UInt(27.W)))
}

class Core16 extends Module {
  val io = IO(new Core16IO)

  val sIdle :: sBusy :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val cnt = RegInit(0.U(4.W))
  val aReg = Reg(Vec(16, UInt(30.W)))
  val bReg = Reg(Vec(16, UInt(16.W)))
  val cReg = Reg(Vec(16, UInt(27.W)))

  io.valid_out := false.B
  io.c := cReg

  switch(state) {
    is(sIdle) {
      when(io.valid_in) {
        for (i <- 0 until 16) {
          aReg(i) := io.a(i)
          bReg(i) := io.b(i)
        }
        cnt := 0.U
        state := sBusy
      }
    }
    is(sBusy) {
      cnt := cnt + 1.U
      when(cnt === 7.U) {
        for (i <- 0 until 16) {
          val aSigned = Cat(aReg(i)(29), aReg(i)).asSInt
          val bSigned = Cat(bReg(i)(15), bReg(i)).asSInt
          cReg(i) := (aSigned * bSigned).asUInt(26, 0)
        }
        state := sDone
      }
    }
    is(sDone) {
      io.valid_out := true.B
      state := sIdle
    }
  }
}