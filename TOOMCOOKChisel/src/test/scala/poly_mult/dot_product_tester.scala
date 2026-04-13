package poly_mult

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class DotTest extends AnyFlatSpec with ChiselScalatestTester {

  private def mask(v: BigInt, width: Int): BigInt = {
    val m = BigInt(1) << width
    ((v % m) + m) % m
  }

  private def signed(v: BigInt, width: Int): BigInt = {
    val x = mask(v, width)
    if (((x >> (width - 1)) & 1) == 1) x - (BigInt(1) << width) else x
  }

  private def mulSignedMq31Q17(a: BigInt, b: BigInt): BigInt = {
    val aPacked = (((a >> 29) & 1) << 30) | (a & ((BigInt(1) << 30) - 1))
    val bPacked = (((b >> 15) & 1) << 16) | (b & ((BigInt(1) << 16) - 1))
    mask(signed(aPacked, 31) * signed(bPacked, 17), 30)
  }

  // ToomCook44 algorithm style reference (same flow as C toomcook44.c)
  private def toomCook44Ref(a: Seq[BigInt], b: Seq[BigInt]): Seq[BigInt] = {
    val A = Array.fill[BigInt](7)(0)
    val B = Array.fill[BigInt](7)(0)
    val w = Array.fill[BigInt](7)(0)

    A(0) = a(3)
    A(1) = a(0) + (a(1) << 1) + (a(2) << 2) + (a(3) << 3)
    A(2) = a(0) + a(1) + a(2) + a(3)
    A(3) = a(0) - a(1) + a(2) - a(3)
    A(4) = (a(0) << 3) + (a(1) << 2) + (a(2) << 1) + a(3)
    A(5) = (a(0) << 3) - (a(1) << 2) + (a(2) << 1) - a(3)
    A(6) = a(0)

    B(0) = b(3)
    B(1) = b(0) + (b(1) << 1) + (b(2) << 2) + (b(3) << 3)
    B(2) = b(0) + b(1) + b(2) + b(3)
    B(3) = b(0) - b(1) + b(2) - b(3)
    B(4) = (b(0) << 3) + (b(1) << 2) + (b(2) << 1) + b(3)
    B(5) = (b(0) << 3) - (b(1) << 2) + (b(2) << 1) - b(3)
    B(6) = b(0)

    for (i <- 0 until 7) {
      w(i) = mulSignedMq31Q17(mask(A(i), 30), mask(B(i), 16))
    }

    val r = Array.fill[BigInt](6)(0)
    r(1) = mask(w(1) + w(4), 30)
    r(5) = mask(w(5) - w(4), 30)
    r(3) = mask((w(3) - w(2)) >> 1, 30)
    r(4) = mask(w(4) - w(0), 30)

    r(4) = mask((r(4) << 1) + r(5) - (w(6) << 7), 30)
    r(2) = mask(w(2) + r(3), 30)
    r(1) = mask(r(1) - (r(2) << 6) - r(2), 30)
    r(2) = mask(r(2) - w(6) - w(0), 30)

    r(1) = mask(r(1) + r(2) + (r(2) << 2) + (r(2) << 3) + (r(2) << 5), 30)
    r(4) =
      mask(mask((r(4) - (r(2) << 3)) >> 3, 30) * BigInt("2AAAAAAB", 16), 30)

    r(5) = mask(r(5) + r(1), 30)
    r(1) =
      mask(mask((r(1) + (r(3) << 4)) >> 1, 30) * BigInt("38E38E39", 16), 30)
    r(2) = mask(r(2) - r(4), 30)

    r(3) = mask(-r(3) - r(1), 30)
    r(5) =
      mask((r(1) - mask((r(5) >> 1) * BigInt("2EEEEEEF", 16), 30)) >> 1, 30)
    r(1) = mask(r(1) - r(5), 30)

    Seq(
      mask(w(6) - r(2), 27),
      mask(r(5) - r(1), 27),
      mask(r(4) - w(0), 27),
      mask(r(3), 27)
    )
  }

  private def fmtHexVec(values: Seq[BigInt]): String =
    values.map(v => f"0x${v.toLong}%X").mkString("[", ", ", "]")

  "dot_product" should "match ToomCook44 C-style reference with uint30/uint16 inputs" in {
    test(new dot_product) { dut =>
      val rng = new Random(20260413L)

      for (_ <- 0 until 20) {
        // direct uint30 / uint16 input generation
        val a = Seq.fill(4)(BigInt(30, rng))
        val b = Seq.fill(4)(BigInt(16, rng))
        val expected = toomCook44Ref(a, b)

        dut.io.valid_in.poke(true.B)
        a.zipWithIndex.foreach { case (v, i) => dut.io.a(i).poke(v.U) }
        b.zipWithIndex.foreach { case (v, i) => dut.io.b(i).poke(v.U) }

        dut.clock.step(1)
        dut.io.valid_in.poke(false.B)

        var seen = false
        for (_ <- 0 until 8) {
          if (dut.io.valid_out.peek().litToBoolean) {
            val got = (0 until 4).map(i => dut.io.c(i).peek().litValue)
            println(s"a(uint30)      = ${fmtHexVec(a)}")
            println(s"b(uint16)      = ${fmtHexVec(b)}")
            println(s"toomcook44(ref)= ${fmtHexVec(expected)}")
            println(s"dot_product    = ${fmtHexVec(got)}")
            println("--------------------------------------------------")

            expected.zip(got).foreach { case (e, g) => assert(g == e) }
            seen = true
          }
          dut.clock.step(1)
        }
        assert(seen, "valid_out was not asserted in expected latency window")
      }
    }
  }
  /*   "ToomCook44" should "show internal values" in {
    test(new dot_product) { dut =>
      dut.io.valid_in.poke(true.B)
      for (i <- 0 until 4) {
        dut.io.a(i).poke((2 * i + 1).U)
        dut.io.b(i).poke((200 * (i + 1)).U)
      }

      // 2. 步进时钟
      // Stage 0 -> Stage 1 需要 1 个周期
      dut.clock.step(1)

      // 3. 步进到 Stage 2 (此时 A_eval 和 B_eval 已经计算出并存入 s1_A, s1_B)
      dut.clock.step(1)

      // 4. 步进到 Stage 3 (此时 w_comb 已经计算出并存入 s2_w)
      dut.clock.step(1)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)
      dut.clock.step(2)
    }
  }*/
}
