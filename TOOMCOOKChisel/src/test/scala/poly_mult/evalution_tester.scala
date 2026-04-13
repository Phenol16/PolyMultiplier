package poly_mult

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class EvalTest extends AnyFlatSpec with ChiselScalatestTester {
  private def mask(value: BigInt, width: Int): BigInt = {
    val mod = BigInt(1) << width
    ((value % mod) + mod) % mod
  }
  private def eval7(values: Seq[BigInt], outWidth: Int): Seq[BigInt] = {
    val r0 = values(0)
    val r1 = values(1)
    val r2 = values(2)
    val r3 = values(3)

    val even = r0 + r2
    val odd = r1 + r3
    val scaledEven = (r0 << 2) + r2
    val scaledOdd = (r1 << 2) + r3

    val h0 = r2 + (r3 << 1)
    val h1 = r1 + (h0 << 1)
    val h2 = r0 + (h1 << 1)

    Seq(
      r3,
      h2,
      even + odd,
      even - odd,
      (scaledEven << 1) + scaledOdd,
      (scaledEven << 1) - scaledOdd,
      r0
    ).map(mask(_, outWidth))
  }

  "evaluation" should "generate expected A_eval/B_eval and pass valid through" in {
    test(new evaluation) { dut =>
      val rng = new Random(20260413L)
      val aIn = Seq.tabulate(16)(i => BigInt(24, rng))
      val bIn = Seq.tabulate(16)(i => BigInt(8, rng))

      dut.io.valid_in.poke(true.B)
      aIn.zipWithIndex.foreach { case (v, i) => dut.io.a(i).poke(v.U) }
      bIn.zipWithIndex.foreach { case (v, i) => dut.io.b(i).poke(v.U) }

      dut.io.valid_out.expect(true.B)

      val expectedA = (0 until 4).flatMap { j =>
        eval7(aIn.slice(j * 4, j * 4 + 4), outWidth = 30)
      }
      val expectedB = (0 until 4).flatMap { j =>
        eval7(bIn.slice(j * 4, j * 4 + 4), outWidth = 16)
      }

      expectedA.zipWithIndex.foreach { case (v, i) =>
        dut.io.A_eval(i).expect(v.U)
      }
      expectedB.zipWithIndex.foreach { case (v, i) =>
        dut.io.B_eval(i).expect(v.U)
      }

      dut.io.valid_in.poke(false.B)
      dut.io.valid_out.expect(false.B)
    }
  }
}
