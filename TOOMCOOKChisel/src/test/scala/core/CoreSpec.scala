package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

object CoreGolden {
  private def mask(w: Int): BigInt = (BigInt(1) << w) - 1
  private def trim(x: BigInt, w: Int): BigInt = x & mask(w)
  private def signed(x: BigInt, w: Int): BigInt = {
    val t = trim(x, w)
    if (t.testBit(w - 1)) t - (BigInt(1) << w) else t
  }
  private def div0(x: BigInt, d: Int): BigInt = x / BigInt(d)

  def eval4(in: Seq[BigInt], inW: Int, outW: Int): Seq[BigInt] = {
    val r = in.map(trim(_, inW))
    val even = r(0) + r(2)
    val odd = r(1) + r(3)
    val scaledEven = (r(0) << 2) + r(2)
    val scaledOdd = (r(1) << 2) + r(3)
    val high0 = r(2) + (r(3) << 1)
    val high1 = r(1) + (high0 << 1)
    val high2 = r(0) + (high1 << 1)
    Seq(
      trim(r(3), outW),
      trim(high2, outW),
      trim(even + odd, outW),
      trim(signed(even - odd, outW), outW),
      trim((scaledEven << 1) + scaledOdd, outW),
      trim(signed((scaledEven << 1) - scaledOdd, outW), outW),
      trim(r(0), outW)
    )
  }

  private def interpColumn(pIn: Seq[BigInt], inW: Int, outW: Int,
                           pr0: BigInt, pr1: BigInt, pr2: BigInt): (Seq[BigInt], BigInt, BigInt, BigInt) = {
    val w = pIn.map(signed(_, inW))
    val c0 = w(6)
    val c1 = div0(-90*w(0) + 2*w(1) - 60*w(2) + 20*w(3) + 5*w(4) - 3*w(5) - 90*w(6), 180)
    val c2 = div0(6*w(0) - 4*w(2) - 4*w(3) + w(4) + w(5) - 120*w(6), 24)
    val c3 = div0(45*w(0) - w(1) + 27*w(2) - 7*w(3) - w(4) + 45*w(6), 18)
    val c4 = div0(-30*w(0) + 16*w(2) + 16*w(3) - w(4) - w(5) + 96*w(6), 24)
    val c5 = div0(-360*w(0) + 8*w(1) - 120*w(2) - 40*w(3) + 5*w(4) + 3*w(5) - 360*w(6), 180)
    val c6 = w(0)
    val outs = Seq(trim(c0 + pr2, outW), trim(c5 + pr1, outW), trim(c4 + pr0, outW), trim(c3, outW))
    (outs, trim(c6, outW), trim(c1, outW), trim(c2, outW))
  }

  def interpLayer(wIn: Seq[BigInt], stride: Int, inW: Int, outW: Int): Seq[BigInt] = {
    val raw = Array.fill(4 * stride)(BigInt(0))
    var pr0 = BigInt(0)
    var pr1 = BigInt(0)
    var pr2 = BigInt(0)
    for (col <- 0 until stride) {
      val points = (0 until 7).map(pt => wIn(pt * stride + col))
      val (outs, nr0, nr1, nr2) = interpColumn(points, inW, outW, pr0, pr1, pr2)
      for (k <- 0 until 4) raw(4 * col + k) = outs(k)
      pr0 = nr0; pr1 = nr1; pr2 = nr2
    }
    raw(0) = trim(raw(0) - pr2, outW)
    raw(1) = trim(raw(1) - pr1, outW)
    raw(2) = trim(raw(2) - pr0, outW)
    raw.toSeq
  }

  def core(params: CoreParams, a: Seq[BigInt], b: Seq[BigInt]): Seq[BigInt] = {
    if (params.degree == 4) {
      val ae = eval4(a, params.aWidth, params.aEvalWidth)
      val be = eval4(b, params.bWidth, params.bEvalWidth)
      val wMul = (0 until 7).map { pt =>
        trim(signed(ae(pt), params.aEvalWidth) * signed(be(pt), params.bEvalWidth), params.productMulWidth)
      }
      interpLayer(wMul, 1, params.productMulWidth, params.outWidth)
    } else {
      val childParams = params.child
      val segmentSize = params.segmentSize
      val childA = Array.fill(7, segmentSize)(BigInt(0))
      val childB = Array.fill(7, segmentSize)(BigInt(0))
      for (col <- 0 until segmentSize) {
        val av = (0 until 4).map(seg => a(seg * segmentSize + col))
        val bv = (0 until 4).map(seg => b(seg * segmentSize + col))
        val ae = eval4(av, params.aWidth, params.aEvalWidth)
        val be = eval4(bv, params.bWidth, params.bEvalWidth)
        for (pt <- 0 until 7) { childA(pt)(col) = ae(pt); childB(pt)(col) = be(pt) }
      }
      val wIn = (0 until 7).flatMap(pt => core(childParams, childA(pt).toSeq, childB(pt).toSeq))
      interpLayer(wIn, segmentSize, childParams.outWidth, params.outWidth)
    }
  }
}

class CoreSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val degrees = Seq(4, 16, 64)
  private val aWidths = Seq(24, 28, 32, 36)
  private val bWidths = Seq(8, 10, 12, 14, 16)

  behavior of "parameterized core.Core"

  it should "elaborate every supported degree and external width combination" in {
    for (d <- degrees; aw <- aWidths; bw <- bWidths) {
      test(new Core(CoreParams(d, aw, bw))) { dut =>
        dut.io.valid_in.poke(false.B)
        dut.clock.step(1)
        dut.io.valid_out.expect(false.B)
      }
    }
  }

  private def runOne(params: CoreParams, a: Seq[BigInt], b: Seq[BigInt]): Unit = {
    test(new Core(params)) { dut =>
      dut.io.valid_in.poke(false.B)
      dut.clock.step(2)
      for (i <- 0 until params.degree) {
        dut.io.avec(i).poke(a(i).U)
        dut.io.bvec(i).poke(b(i).U)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)
      if (params.expectedLatency > 1) dut.clock.step(params.expectedLatency - 1)
      dut.io.valid_out.expect(true.B)
      val expected = CoreGolden.core(params, a, b)
      for (i <- 0 until params.degree) dut.io.cOut(i).expect(expected(i).U)
      dut.clock.step(1)
      dut.io.valid_out.expect(false.B)
    }
  }

  it should "pass deterministic corner samples for each degree" in {
    for (d <- degrees) {
      val params = CoreParams(d, 24, 8)
      val maxA = (BigInt(1) << params.aWidth) - 1
      val maxB = (BigInt(1) << params.bWidth) - 1
      val samples = Seq(
        (Seq.fill(d)(BigInt(0)), Seq.fill(d)(BigInt(0))),
        (Seq.fill(d)(BigInt(1)), Seq.fill(d)(BigInt(1))),
        ((0 until d).map(i => if (i == 0) BigInt(1) else BigInt(0)), (0 until d).map(i => if (i == d - 1) BigInt(1) else BigInt(0))),
        ((0 until d).map(i => BigInt(i + 1)), (0 until d).map(i => BigInt(2 * i + 1))),
        (Seq.fill(d)(maxA), Seq.fill(d)(maxB))
      )
      samples.foreach { case (a, b) => runOne(params, a, b) }
    }
  }

  it should "pass random tests for every supported parameter combination" in {
    val rng = new Random(20260507L)
    for (d <- degrees; aw <- aWidths; bw <- bWidths) {
      val params = CoreParams(d, aw, bw)
      val maxA = BigInt(1) << aw
      val maxB = BigInt(1) << bw
      for (_ <- 0 until 20) {
        val a = Seq.fill(d)(BigInt(aw, rng) % maxA)
        val b = Seq.fill(d)(BigInt(bw, rng) % maxB)
        runOne(params, a, b)
      }
    }
  }
}
