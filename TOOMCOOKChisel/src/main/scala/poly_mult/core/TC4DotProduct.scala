package poly_mult

import chisel3._
import chisel3.util._

/** Signed two's-complement multiplier used by TC4 dot/sub-cores. */
class TC4SignedMulUnit(aW: Int, bW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(aW.W))
    val b = Input(UInt(bW.W))
    val c = Output(UInt(outW.W))
  })

  val aSigned = io.a.asSInt
  val bSigned = io.b.asSInt
  io.c := TC4BitUtil.low((aSigned * bSigned).asUInt, outW)
}

class TC4DotProductIO(val aW: Int, val bW: Int, val outW: Int) extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(4, UInt(aW.W)))
  val b = Input(Vec(4, UInt(bW.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(4, UInt(outW.W)))
}

/**
  * Parameterized 4-coefficient dot_product from the original toomcook16 path.
  *
  * Internally it still performs Toom-Cook-4 evaluation, seven signed pointwise
  * multiplications, and one-column interpolation; only the widths are
  * parameters rather than the original fixed 30/16/30/27 values.
  */
class TC4DotProduct(aW: Int, bW: Int, aEvalW: Int, bEvalW: Int, mulOutW: Int, outW: Int)
    extends Module {
  require(aEvalW >= aW + 3)
  require(bEvalW >= bW + 3)
  require(mulOutW >= outW)

  val io = IO(new TC4DotProductIO(aW, bW, outW))

  val aGrouped = Wire(Vec(4, Vec(1, UInt(aW.W))))
  val bGrouped = Wire(Vec(4, Vec(1, UInt(bW.W))))
  for (i <- 0 until 4) {
    aGrouped(i)(0) := io.a(i)
    bGrouped(i)(0) := io.b(i)
  }

  val aEval = Module(new TC4Eval(aW, aEvalW, 1, signedInputs = true))
  val bEval = Module(new TC4Eval(bW, bEvalW, 1, signedInputs = true))
  aEval.io.valid_in := io.valid_in
  bEval.io.valid_in := io.valid_in
  aEval.io.x := aGrouped
  bEval.io.x := bGrouped

  val s1Valid = RegNext(aEval.io.valid_out, false.B)
  val s1A = RegNext(aEval.io.y)
  val s1B = RegNext(bEval.io.y)

  val mulUnits = Seq.fill(7)(Module(new TC4SignedMulUnit(aEvalW, bEvalW, mulOutW)))
  for (i <- 0 until 7) {
    mulUnits(i).io.a := s1A(i)(0)
    mulUnits(i).io.b := s1B(i)(0)
  }

  val s2Valid = RegNext(s1Valid, false.B)
  val s2W = RegNext(VecInit(mulUnits.map(_.io.c)))

  val interp = Module(new TC4Interpolation(1, mulOutW, mulOutW, outW))
  interp.io.valid_in := s2Valid
  for (i <- 0 until 7) {
    interp.io.w(i)(0) := s2W(i)
  }

  io.valid_out := interp.io.valid_out
  io.c := interp.io.c
}
