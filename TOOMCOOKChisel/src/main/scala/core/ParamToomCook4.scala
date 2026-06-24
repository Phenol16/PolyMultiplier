package core

import chisel3._

class ParamToomCook4(aW: Int, bW: Int, outW: Int) extends Module {
  require(aW > 0, "aW must be positive")
  require(bW > 0, "bW must be positive")
  require(outW > 0, "outW must be positive")

  val aEvalW: Int = aW + 4
  val bEvalW: Int = bW + 4

  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(4, UInt(aW.W)))
    val b = Input(Vec(4, UInt(bW.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(4, UInt(outW.W)))
  })

  val aEval = ParamEval4(io.a, aW, aEvalW)
  val bEval = ParamEval4(io.b, bW, bEvalW)

  val s1Valid = RegNext(io.valid_in, false.B)
  val s1A = RegNext(aEval)
  val s1B = RegNext(bEval)

  val w = Wire(Vec(7, UInt(outW.W)))
  for (i <- 0 until 7) {
    val mul = Module(new ParamSignedMul(aEvalW, bEvalW, outW))
    mul.io.a := s1A(i)
    mul.io.b := s1B(i)
    w(i) := mul.io.c
  }

  val s2Valid = RegNext(s1Valid, false.B)
  val s2W = RegNext(w)

  val cComb = ParamInterp4.negacyclic4(s2W, outW)
  io.valid_out := RegNext(s2Valid, false.B)
  io.c := RegNext(cComb)
}
