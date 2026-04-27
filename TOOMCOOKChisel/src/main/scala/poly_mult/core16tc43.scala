package poly_mult

import chisel3._
import chisel3.util._

class Core16TC43IO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(16, UInt(30.W)))
  val b = Input(Vec(16, UInt(16.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(16, UInt(27.W)))
}

// 占位核心：多拍完成，不做内部流水
class Core16TC43 extends Module {
  val io = IO(new Core16TC43IO)

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
