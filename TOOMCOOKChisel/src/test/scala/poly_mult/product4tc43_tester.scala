package poly_mult

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class Product4TC43Test extends AnyFlatSpec with ChiselScalatestTester {

  private val AWidth = 36
  private val BWidth = 24
  private val OutWidth = 36
  private val N = 4

  // 最终系数模数 q = 2^24
  // Product4TC43 的 io.out 虽然是 36 bit，但真正需要验的是低 24 bit。
  private val QWidth = 36
  private val QMask: BigInt = (BigInt(1) << QWidth) - 1

  private val MaskA: BigInt = (BigInt(1) << AWidth) - 1
  private val MaskB: BigInt = (BigInt(1) << BWidth) - 1
  private val MaskOut: BigInt = (BigInt(1) << OutWidth) - 1

  /**
   * 参考模型：
   *
   * 计算 4 项负循环卷积：
   *
   *   c(x) = a(x) * b(x) mod (x^4 + 1)
   *
   * 即：
   *
   *   x^4 = -1
   *   x^5 = -x
   *   x^6 = -x^2
   *
   * 普通线性卷积为：
   *
   *   d0 = a0b0
   *   d1 = a0b1 + a1b0
   *   d2 = a0b2 + a1b1 + a2b0
   *   d3 = a0b3 + a1b2 + a2b1 + a3b0
   *   d4 = a1b3 + a2b2 + a3b1
   *   d5 = a2b3 + a3b2
   *   d6 = a3b3
   *
   * 负回绕后：
   *
   *   c0 = d0 - d4
   *   c1 = d1 - d5
   *   c2 = d2 - d6
   *   c3 = d3
   *
   * 最终按 q = 2^24 取模。
   */
  private def schoolbookNegacyclicModQ(
      a: Seq[BigInt],
      b: Seq[BigInt],
      n: Int = N
  ): Seq[BigInt] = {
    require(n == 4, "Product4TC43 test only supports n = 4")
    require(a.length == 4, "a length must be 4")
    require(b.length == 4, "b length must be 4")

    val c = Array.fill(n)(BigInt(0))

    for (i <- 0 until n) {
      for (j <- 0 until n) {
        val degree = i + j
        val product = a(i) * b(j)

        if (degree < n) {
          c(degree) = c(degree) + product
        } else {
          // x^4 = -1，所以高次项负回绕到 degree - 4。
          c(degree - n) = c(degree - n) - product
        }
      }
    }

    // 这里非常关键：
    // 最终验证的是 q = 2^24 环上的结果，而不是 2^36 环上的结果。
    c.map(x => x & QMask).toSeq
  }

  private def hex(x: BigInt): String = s"0x${x.toString(16)}"

  private def runCase(
      dut: Product4TC43,
      label: String,
      a: Seq[BigInt],
      b: Seq[BigInt],
      printAll: Boolean = false
  ): Unit = {
    require(a.length == 4, s"[$label] a length must be 4")
    require(b.length == 4, s"[$label] b length must be 4")

    val expected = schoolbookNegacyclicModQ(a, b)

    println(s"========== Product4TC43 case: $label ==========")

    for (i <- 0 until 4) {
      dut.io.a4(i).poke((a(i) & MaskA).U)
      dut.io.b4(i).poke((b(i) & MaskB).U)
    }

    // Product4TC43 是组合模块，这里 step 1 拍只是为了让测试行为稳定、日志清楚。
    dut.clock.step(1)

    var mismatchCount = 0

    // Product4TC43 现在已经完成 x^4 = -1 回绕，只输出 4 项。
    for (i <- 0 until 4) {
      val gotRaw = dut.io.out(i).peek().litValue
      val gotOutW = gotRaw & MaskOut

      // 真正要比较的是低 24 位，即模 q = 2^24 的结果。
      val gotModQ = gotRaw & QMask
      val expModQ = expected(i) & QMask

      if (printAll) {
        println(
          s"[$label] out[$i] raw36=${hex(gotOutW)} modQ=${hex(gotModQ)} expectedQ=${hex(expModQ)}"
        )
      }

      if (gotModQ != expModQ) {
        mismatchCount += 1
        println(
          s"[$label MISMATCH] out[$i] raw36=${hex(gotOutW)} modQ=${hex(gotModQ)} expectedQ=${hex(expModQ)}"
        )
      }
    }

    assert(
      mismatchCount == 0,
      s"[$label] Product4TC43 mismatchCount=$mismatchCount"
    )

    println(s"[$label] PASS")
  }

  behavior of "Product4TC43"

  it should "match 4-term negacyclic multiplication modulo x^4 + 1 and q = 2^24" in {
    test(new Product4TC43(aInW = AWidth, bInW = BWidth))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

        dut.clock.setTimeout(0)

        // ---------------------------------------------------------------------
        // Case 0: 全 0
        // expected = [0, 0, 0, 0]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "zero",
          a = Seq(0, 0, 0, 0).map(BigInt(_)),
          b = Seq(0, 0, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 1: a0 * b0
        // 1 * 1 = 1
        // expected = [1, 0, 0, 0]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a0_b0_no_wrap",
          a = Seq(1, 0, 0, 0).map(BigInt(_)),
          b = Seq(1, 0, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 2: a1 * b0
        // x * 1 = x
        // expected = [0, 1, 0, 0]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a1_b0_no_wrap",
          a = Seq(0, 1, 0, 0).map(BigInt(_)),
          b = Seq(1, 0, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 3: a0 * b1
        // 1 * x = x
        // expected = [0, 1, 0, 0]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a0_b1_no_wrap",
          a = Seq(1, 0, 0, 0).map(BigInt(_)),
          b = Seq(0, 1, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 4: a3 * b0
        // x^3 * 1 = x^3
        // expected = [0, 0, 0, 1]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a3_b0_no_wrap",
          a = Seq(0, 0, 0, 1).map(BigInt(_)),
          b = Seq(1, 0, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 5: a3 * b1
        // x^3 * x = x^4 = -1
        // expected = [-1, 0, 0, 0] mod 2^24
        // 即 expected = [0xffffff, 0, 0, 0]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a3_b1_wrap_to_c0_negative",
          a = Seq(0, 0, 0, 1).map(BigInt(_)),
          b = Seq(0, 1, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 6: a2 * b2
        // x^2 * x^2 = x^4 = -1
        // expected = [-1, 0, 0, 0] mod 2^24
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a2_b2_wrap_to_c0_negative",
          a = Seq(0, 0, 1, 0).map(BigInt(_)),
          b = Seq(0, 0, 1, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 7: a3 * b2
        // x^3 * x^2 = x^5 = -x
        // expected = [0, -1, 0, 0] mod 2^24
        // 即 expected = [0, 0xffffff, 0, 0]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a3_b2_wrap_to_c1_negative",
          a = Seq(0, 0, 0, 1).map(BigInt(_)),
          b = Seq(0, 0, 1, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 8: a3 * b3
        // x^3 * x^3 = x^6 = -x^2
        // expected = [0, 0, -1, 0] mod 2^24
        // 即 expected = [0, 0, 0xffffff, 0]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a3_b3_wrap_to_c2_negative",
          a = Seq(0, 0, 0, 1).map(BigInt(_)),
          b = Seq(0, 0, 0, 1).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 9: 小整数手算 case
        //
        // a = [1, 2, 3, 4]
        // b = [5, 6, 7, 8]
        //
        // 普通线性卷积：
        // d = [5, 16, 34, 60, 61, 52, 32]
        //
        // 按 x^4 = -1 负回绕：
        // c0 = d0 - d4 = 5  - 61 = -56
        // c1 = d1 - d5 = 16 - 52 = -36
        // c2 = d2 - d6 = 34 - 32 = 2
        // c3 = d3      = 60
        //
        // expected = [-56, -36, 2, 60] mod 2^24
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "small_manual_negacyclic",
          a = Seq(1, 2, 3, 4).map(BigInt(_)),
          b = Seq(5, 6, 7, 8).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 10: 回绕抵消 case
        //
        // a = 1 + x^3
        // b = 1 + x
        //
        // 普通乘法：
        // (1 + x^3)(1 + x) = 1 + x + x^3 + x^4
        //
        // x^4 = -1：
        // 1 + x + x^3 - 1 = x + x^3
        //
        // expected = [0, 1, 0, 1]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "wrap_cancellation",
          a = Seq(1, 0, 0, 1).map(BigInt(_)),
          b = Seq(1, 1, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 11: 小随机。
        // ---------------------------------------------------------------------
        val rngSmall = new Random(7)
        for (caseId <- 0 until 20) {
          val a = Seq.fill(4)(BigInt(rngSmall.nextInt(16)))
          val b = Seq.fill(4)(BigInt(rngSmall.nextInt(16)))

          runCase(
            dut = dut,
            label = s"small_random_$caseId",
            a = a,
            b = b,
            printAll = false
          )
        }

        // ---------------------------------------------------------------------
        // Case 12: 接近真实输入范围的随机。
        //
        // a 输入宽度为 36 bit，b 输入宽度为 24 bit。
        // 但最终验的是 q = 2^24 意义下的结果。
        // 因此 a 的高 12 位可以参与输入压力测试，但比较时只比较最终低 24 位。
        // ---------------------------------------------------------------------
        val rngFull = new Random(42)
        for (caseId <- 0 until 50) {
          val a = Seq.fill(4)(BigInt(AWidth, rngFull))
          val b = Seq.fill(4)(BigInt(BWidth, rngFull))

          runCase(
            dut = dut,
            label = s"full_random_$caseId",
            a = a,
            b = b,
            printAll = false
          )
        }

        // ---------------------------------------------------------------------
        // Case 13: 最大值边界测试。
        //
        // 注意：
        // 这里不是要求 out 的高 12 位为 0。
        // 这里只验证 out 的低 24 位是否等于模 q 后的结果。
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "max_value_boundary",
          a = Seq.fill(4)(MaskA),
          b = Seq.fill(4)(MaskB),
          printAll = true
        )
      }
  }
}