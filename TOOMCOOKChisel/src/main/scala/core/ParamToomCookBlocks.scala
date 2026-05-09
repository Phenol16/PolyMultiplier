package core

import chisel3._
import chisel3.util.log2Ceil

object ParamToomCookBlocks {
  private def signedCoeff(value: UInt, width: Int): SInt =
    CoreBitUtil.signExtend(value, width + 1).asSInt

  private def resizeSigned(value: SInt, width: Int): SInt =
    CoreBitUtil.signExtend(value.asUInt, width).asSInt

  def fullConvolution(a: Seq[UInt], b: Seq[UInt], aW: Int, bW: Int, outW: Int): Vec[UInt] = {
    require(a.nonEmpty, "a must be non-empty")
    require(a.length == b.length, s"pointwise lengths differ: a=${a.length}, b=${b.length}")
    val n = a.length
    val accW = outW + log2Ceil(n + 1) + 2
    val aSigned = a.map(signedCoeff(_, aW))
    val bSigned = b.map(signedCoeff(_, bW))
    val out = Wire(Vec(2 * n - 1, UInt(outW.W)))

    for (k <- 0 until (2 * n - 1)) {
      val terms = for {
        i <- 0 until n
        j = k - i
        if j >= 0 && j < n
      } yield resizeSigned(aSigned(i) * bSigned(j), accW)
      val sum = terms.reduceOption(_ +& _).getOrElse(0.S(accW.W))
      out(k) := CoreBitUtil.mask(sum.asUInt, outW)
    }
    out
  }

  def foldOuterProduct(coeffByInner: Seq[Seq[UInt]], blockN: Int, outW: Int): Vec[UInt] = {
    require(coeffByInner.length == 2 * blockN - 1, s"expected ${2 * blockN - 1} inner columns")
    coeffByInner.foreach(col => require(col.length == 7, "each inner column must contain 7 outer coefficients"))

    val n = blockN * 4
    val out = Wire(Vec(n, UInt(outW.W)))
    for (dst <- 0 until n) {
      val terms = for {
        inner <- 0 until (2 * blockN - 1)
        outer <- 0 until 7
        idx = outer * blockN + inner
        if idx == dst || idx - n == dst
      } yield {
        if (idx < n) coeffByInner(inner)(outer) else CoreBitUtil.mask(0.U(outW.W) - coeffByInner(inner)(outer), outW)
      }
      out(dst) := terms.foldLeft(0.U(outW.W))((acc, term) => CoreBitUtil.mask(acc + term, outW))
    }
    out
  }
}

class ParamToomCook16Block(aW: Int, bW: Int, outW: Int) extends Module {
  require(aW > 0, "aW must be positive")
  require(bW > 0, "bW must be positive")
  require(outW > 0, "outW must be positive")

  private val blockN = 4
  private val aEvalW = aW + 4
  private val bEvalW = bW + 4

  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(16, UInt(aW.W)))
    val b = Input(Vec(16, UInt(bW.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(16, UInt(outW.W)))
  })

  val aEval = Seq.tabulate(blockN)(j => ParamEval4(Seq.tabulate(4)(k => io.a(k * blockN + j)), aW, aEvalW))
  val bEval = Seq.tabulate(blockN)(j => ParamEval4(Seq.tabulate(4)(k => io.b(k * blockN + j)), bW, bEvalW))

  val pointwise = Seq.tabulate(7) { p =>
    ParamToomCookBlocks.fullConvolution(
      Seq.tabulate(blockN)(j => aEval(j)(p)),
      Seq.tabulate(blockN)(j => bEval(j)(p)),
      aEvalW,
      bEvalW,
      outW
    )
  }

  val s1Valid = RegNext(io.valid_in, false.B)
  val s1Pointwise = RegNext(VecInit(pointwise))

  val coeffByInner = Seq.tabulate(2 * blockN - 1) { inner =>
    ParamInterp4.productCoeffs(Seq.tabulate(7)(p => s1Pointwise(p)(inner)), outW)
  }
  val cComb = ParamToomCookBlocks.foldOuterProduct(coeffByInner, blockN, outW)

  io.valid_out := RegNext(s1Valid, false.B)
  io.c := RegNext(cComb)
}

class ParamToomCook64Block(aW: Int, bW: Int, outW: Int) extends Module {
  require(aW > 0, "aW must be positive")
  require(bW > 0, "bW must be positive")
  require(outW > 0, "outW must be positive")

  private val blockN = 16
  private val aEvalW = aW + 4
  private val bEvalW = bW + 4

  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(64, UInt(aW.W)))
    val b = Input(Vec(64, UInt(bW.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(64, UInt(outW.W)))
  })

  val aEval = Seq.tabulate(blockN)(j => ParamEval4(Seq.tabulate(4)(k => io.a(k * blockN + j)), aW, aEvalW))
  val bEval = Seq.tabulate(blockN)(j => ParamEval4(Seq.tabulate(4)(k => io.b(k * blockN + j)), bW, bEvalW))

  val pointwise = Seq.tabulate(7) { p =>
    ParamToomCookBlocks.fullConvolution(
      Seq.tabulate(blockN)(j => aEval(j)(p)),
      Seq.tabulate(blockN)(j => bEval(j)(p)),
      aEvalW,
      bEvalW,
      outW
    )
  }

  val s1Valid = RegNext(io.valid_in, false.B)
  val s1Pointwise = RegNext(VecInit(pointwise))

  val coeffByInner = Seq.tabulate(2 * blockN - 1) { inner =>
    ParamInterp4.productCoeffs(Seq.tabulate(7)(p => s1Pointwise(p)(inner)), outW)
  }
  val cComb = ParamToomCookBlocks.foldOuterProduct(coeffByInner, blockN, outW)

  io.valid_out := RegNext(s1Valid, false.B)
  io.c := RegNext(cComb)
}
