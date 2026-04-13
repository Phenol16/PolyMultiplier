package poly_mult

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class DotProductTest extends AnyFlatSpec with ChiselScalatestTester {
  private def mask(v: BigInt, width: Int): BigInt = {
    val m = BigInt(1) << width
    ((v % m) + m) % m
  }

  private def signed(v: BigInt, width: Int): BigInt = {
    val x = mask(v, width)
    if (((x >> (width - 1)) & 1) == 1) x - (BigInt(1) << width) else x
  }

  private def mulSignedMq28Q13(a: BigInt, b: BigInt): BigInt = {
    val aSignBit = (a >> 29) & 1
    val bSignBit = (b >> 15) & 1
    val aPacked = (aSignBit << 27) | (a & ((BigInt(1) << 27) - 1))
    val bPacked = (bSignBit << 12) | (b & ((BigInt(1) << 12) - 1))
    mask(signed(aPacked, 28) * signed(bPacked, 13), 27)
  }

  private def referenceDot(a: Seq[BigInt], b: Seq[BigInt]): Seq[BigInt] = {
    val c = Array.fill[BigInt](4)(0)
    for (i <- 0 until 4) {
      for (j <- i until 4) {
        val prod = mulSignedMq28Q13(a(i), b(j - i))
        c(j) = mask(c(j) + prod, 27)
      }
      for (j <- 0 until i) {
        val prod = mulSignedMq28Q13(a(i), b(4 + j - i))
        c(j) = mask(c(j) - prod, 27)
      }
    }
    c.toSeq
  }

  "dot_product" should "match software model" in {
    test(new dot_product) { dut =>
      val rng = new Random(20260413L)

      for (_ <- 0 until 20) {
        val a = Seq.fill(4)(BigInt(30, rng))
        val b = Seq.fill(4)(BigInt(16, rng))
        val expected = referenceDot(a, b)

        dut.io.valid_in.poke(true.B)
        a.zipWithIndex.foreach { case (v, i) => dut.io.a(i).poke(v.U) }
        b.zipWithIndex.foreach { case (v, i) => dut.io.b(i).poke(v.U) }

        dut.clock.step(1)
        dut.io.valid_in.poke(false.B)

        var seen = false
        for (_ <- 0 until 8) {
          if (dut.io.valid_out.peek().litToBoolean) {
            expected.zipWithIndex.foreach { case (v, i) => dut.io.c(i).expect(v.U) }
            seen = true
          }
          dut.clock.step(1)
        }
        assert(seen, "valid_out was not asserted in expected latency window")
      }
    }
  }
}
