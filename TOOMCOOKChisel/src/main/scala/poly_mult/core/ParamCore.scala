package poly_mult

import chisel3._
import chisel3.util._

class ParamCoreIO(val p: CoreParams) extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(p.n, UInt(p.aInW.W)))
  val b = Input(Vec(p.n, UInt(p.bInW.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(p.n, UInt(p.outW.W)))
}

/**
  * Public parameterized Toom-Cook-4 core.
  *
  * This module preserves the original toomcook16 kernel structure:
  * evaluation -> seven parallel dot/sub-cores -> interpolation -> output
  * register. n=4 uses one-column interpolation directly after seven signed
  * multipliers; n=16 uses seven TC4DotProduct modules; n=64 uses seven n=16
  * sub-cores with the same structural pattern.
  */
class ParamCore(p: CoreParams, signedInputs: Boolean = false) extends Module {
  val io = IO(new ParamCoreIO(p))

  private val subLen = p.subLen

  val aGrouped = Wire(Vec(4, Vec(subLen, UInt(p.aInW.W))))
  val bGrouped = Wire(Vec(4, Vec(subLen, UInt(p.bInW.W))))
  for (g <- 0 until 4; j <- 0 until subLen) {
    aGrouped(g)(j) := io.a(j * 4 + g)
    bGrouped(g)(j) := io.b(j * 4 + g)
  }

  val aEval = Module(new TC4Eval(p.aInW, p.aEvalW, subLen, signedInputs))
  val bEval = Module(new TC4Eval(p.bInW, p.bEvalW, subLen, signedInputs))
  aEval.io.valid_in := io.valid_in
  bEval.io.valid_in := io.valid_in
  aEval.io.x := aGrouped
  bEval.io.x := bGrouped

  if (p.n == 4) {
    val s1Valid = RegNext(aEval.io.valid_out, false.B)
    val s1A = RegNext(aEval.io.y)
    val s1B = RegNext(bEval.io.y)

    val mulUnits = Seq.fill(7)(Module(new TC4SignedMulUnit(p.aEvalW, p.bEvalW, p.mulOutW)))
    for (i <- 0 until 7) {
      mulUnits(i).io.a := s1A(i)(0)
      mulUnits(i).io.b := s1B(i)(0)
    }

    val s2Valid = RegNext(s1Valid, false.B)
    val s2W = RegNext(VecInit(mulUnits.map(_.io.c)))

    val interp = Module(new TC4Interpolation(1, p.mulOutW, p.interpW, p.outW))
    interp.io.valid_in := s2Valid
    for (i <- 0 until 7) interp.io.w(i)(0) := s2W(i)

    io.valid_out := interp.io.valid_out
    io.c := interp.io.c
  } else if (p.n == 16) {
    val dots = Seq.fill(7)(Module(new TC4DotProduct(
      p.aEvalW,
      p.bEvalW,
      p.aEvalW + 6,
      p.bEvalW + 6,
      p.mulOutW,
      p.mulOutW
    )))

    for (i <- 0 until 7) {
      dots(i).io.valid_in := aEval.io.valid_out
      for (j <- 0 until subLen) {
        dots(i).io.a(j) := aEval.io.y(i)(j)
        dots(i).io.b(j) := bEval.io.y(i)(j)
      }
    }

    val dotValidReg = RegNext(dots.head.io.valid_out, false.B)
    val dotCWire = Wire(Vec(7, Vec(subLen, UInt(p.mulOutW.W))))
    for (i <- 0 until 7; j <- 0 until subLen) dotCWire(i)(j) := dots(i).io.c(j)
    val dotCReg = RegNext(dotCWire)

    val interp = Module(new TC4Interpolation(subLen, p.mulOutW, p.interpW, p.outW))
    interp.io.valid_in := dotValidReg
    interp.io.w := dotCReg

    io.valid_out := interp.io.valid_out
    io.c := interp.io.c
  } else {
    val subParams = CoreParams(
      n = 16,
      aInW = p.aEvalW,
      bInW = p.bEvalW,
      aEvalW = p.aEvalW + 6,
      bEvalW = p.bEvalW + 6,
      mulOutW = p.mulOutW,
      interpW = p.interpW,
      outW = p.mulOutW
    )
    val subs = Seq.fill(7)(Module(new ParamCore(subParams, signedInputs = true)))
    for (i <- 0 until 7) {
      subs(i).io.valid_in := aEval.io.valid_out
      for (j <- 0 until subLen) {
        subs(i).io.a(j) := aEval.io.y(i)(j)
        subs(i).io.b(j) := bEval.io.y(i)(j)
      }
    }

    val subValidReg = RegNext(subs.head.io.valid_out, false.B)
    val subCWire = Wire(Vec(7, Vec(subLen, UInt(p.mulOutW.W))))
    for (i <- 0 until 7; j <- 0 until subLen) subCWire(i)(j) := subs(i).io.c(j)
    val subCReg = RegNext(subCWire)

    val interp = Module(new TC4Interpolation(subLen, p.mulOutW, p.interpW, p.outW))
    interp.io.valid_in := subValidReg
    interp.io.w := subCReg

    io.valid_out := interp.io.valid_out
    io.c := interp.io.c
  }
}
