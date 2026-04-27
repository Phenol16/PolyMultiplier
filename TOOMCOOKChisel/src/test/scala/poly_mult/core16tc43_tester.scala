package poly_mult

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class Core16TC43Test extends AnyFlatSpec with ChiselScalatestTester {

  // ---------------------------------------------------------------------------
  // Core16TC43 的原始输入宽度
  //
  // 如果你的顶层 16 阶核输入是：
  //   a: 24 bit
  //   b: 8 bit
  //
  // 就保持下面这个配置。
  //
  // 如果你实际测试的是别的宽度，例如 aInW = 36, bInW = 24，
  // 那么只需要改这里。
  // ---------------------------------------------------------------------------
  private val AWidth = 33
  private val BWidth = 20

  private val OutWidth = 33
  private val N = 16

  // 最终多项式系数模数 q = 2^24。
  // cOut 虽然是 36 bit，但测试时默认比较低 24 bit。
  private val QWidth = 33
  private val QMask: BigInt = (BigInt(1) << QWidth) - 1

  private val MaskA: BigInt = (BigInt(1) << AWidth) - 1
  private val MaskB: BigInt = (BigInt(1) << BWidth) - 1
  private val MaskOut: BigInt = (BigInt(1) << OutWidth) - 1

  private def hex(x: BigInt): String = s"0x${x.toString(16)}"

  /**
   * 16 项负循环卷积参考模型：
   *
   *   c(x) = a(x) * b(x) mod (x^16 + 1)
   *
   * 也就是：
   *
   *   x^16 = -1
   *   x^17 = -x
   *   x^18 = -x^2
   *   ...
   *
   * 对于普通线性卷积中 degree >= 16 的项：
   *
   *   x^degree = -x^(degree - 16)
   *
   * 最后按照 q = 2^24 取模。
   */
  private def schoolbookNegacyclic16ModQ(
      a: Seq[BigInt],
      b: Seq[BigInt]
  ): Seq[BigInt] = {
    require(a.length == N, s"a length must be $N")
    require(b.length == N, s"b length must be $N")

    val c = Array.fill(N)(BigInt(0))

    for (i <- 0 until N) {
      for (j <- 0 until N) {
        val degree = i + j
        val product = a(i) * b(j)

        if (degree < N) {
          c(degree) = c(degree) + product
        } else {
          // x^16 = -1，因此高次项负回绕到 degree - 16。
          c(degree - N) = c(degree - N) - product
        }
      }
    }

    c.map(x => x & QMask).toSeq
  }

  /**
   * 单个 case 的测试函数。
   *
   * Core16TC43 内部有：
   *
   *   val regW     = RegEnable(wProd, io.valid_in)
   *   val regValid = RegNext(io.valid_in, false.B)
   *
   * 因此：
   *
   *   第 t 拍输入 valid_in = true
   *   step 1 拍之后 valid_out = true
   *   此时读取 cOut
   */
  private def runCase(
      dut: Core16TC43,
      label: String,
      a: Seq[BigInt],
      b: Seq[BigInt],
      printAll: Boolean = false
  ): Unit = {
    require(a.length == N, s"[$label] a length must be $N")
    require(b.length == N, s"[$label] b length must be $N")

    val expected = schoolbookNegacyclic16ModQ(a, b)

    println(s"========== Core16TC43 case: $label ==========")

    // 输入有效，写入 16 项 a、b。
    dut.io.valid_in.poke(true.B)

    for (i <- 0 until N) {
      dut.io.avec(i).poke((a(i) & MaskA).U)
      dut.io.bvec(i).poke((b(i) & MaskB).U)
    }

    // Core16TC43 只有一级寄存器：
    // valid_in 拉高后，step 1 拍，valid_out 应该拉高。
    dut.clock.step(1)

    dut.io.valid_out.expect(true.B)

    var mismatchCount = 0

    for (i <- 0 until N) {
      val gotRaw = dut.io.cOut(i).peek().litValue
      val gotOutW = gotRaw & MaskOut

      // 这里默认验证最终模 q = 2^24 的结果。
      val gotModQ = gotRaw & QMask
      val expModQ = expected(i) & QMask

      if (printAll) {
        println(
          s"[$label] cOut[$i] raw36=${hex(gotOutW)} modQ=${hex(gotModQ)} expectedQ=${hex(expModQ)}"
        )
      }

      if (gotModQ != expModQ) {
        mismatchCount += 1
        println(
          s"[$label MISMATCH] cOut[$i] raw36=${hex(gotOutW)} modQ=${hex(gotModQ)} expectedQ=${hex(expModQ)}"
        )
      }
    }

    assert(
      mismatchCount == 0,
      s"[$label] Core16TC43 mismatchCount=$mismatchCount"
    )

    println(s"[$label] PASS")

    // 拉低 valid_in，顺手检查 valid_out 下一拍会回到 false。
    dut.io.valid_in.poke(false.B)
    dut.clock.step(1)
    dut.io.valid_out.expect(false.B)
  }

  behavior of "Core16TC43"

  it should "match 16-term negacyclic schoolbook multiplication modulo x^16 + 1 and q = 2^24" in {
    test(new Core16TC43(aInW = AWidth, bInW = BWidth))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

        dut.clock.setTimeout(0)

        // ---------------------------------------------------------------------
        // Case 0: 全 0
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "zero",
          a = Seq.fill(N)(BigInt(0)),
          b = Seq.fill(N)(BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 1: a0 * b0
        //
        // 1 * 1 = 1
        // expected:
        //   c[0] = 1
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a0_b0_no_wrap",
          a = Seq.tabulate(N)(i => if (i == 0) BigInt(1) else BigInt(0)),
          b = Seq.tabulate(N)(i => if (i == 0) BigInt(1) else BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 2: a1 * b0
        //
        // x * 1 = x
        // expected:
        //   c[1] = 1
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a1_b0_no_wrap",
          a = Seq.tabulate(N)(i => if (i == 1) BigInt(1) else BigInt(0)),
          b = Seq.tabulate(N)(i => if (i == 0) BigInt(1) else BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 3: a0 * b1
        //
        // 1 * x = x
        // expected:
        //   c[1] = 1
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a0_b1_no_wrap",
          a = Seq.tabulate(N)(i => if (i == 0) BigInt(1) else BigInt(0)),
          b = Seq.tabulate(N)(i => if (i == 1) BigInt(1) else BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 4: a15 * b0
        //
        // x^15 * 1 = x^15
        // expected:
        //   c[15] = 1
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a15_b0_no_wrap",
          a = Seq.tabulate(N)(i => if (i == 15) BigInt(1) else BigInt(0)),
          b = Seq.tabulate(N)(i => if (i == 0) BigInt(1) else BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 5: a15 * b1
        //
        // x^15 * x = x^16 = -1
        // expected:
        //   c[0] = -1 mod 2^24 = 0xffffff
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a15_b1_wrap_to_c0_negative",
          a = Seq.tabulate(N)(i => if (i == 15) BigInt(1) else BigInt(0)),
          b = Seq.tabulate(N)(i => if (i == 1) BigInt(1) else BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 6: a14 * b2
        //
        // x^14 * x^2 = x^16 = -1
        // expected:
        //   c[0] = -1 mod 2^24 = 0xffffff
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a14_b2_wrap_to_c0_negative",
          a = Seq.tabulate(N)(i => if (i == 14) BigInt(1) else BigInt(0)),
          b = Seq.tabulate(N)(i => if (i == 2) BigInt(1) else BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 7: a15 * b2
        //
        // x^15 * x^2 = x^17 = -x
        // expected:
        //   c[1] = -1 mod 2^24 = 0xffffff
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a15_b2_wrap_to_c1_negative",
          a = Seq.tabulate(N)(i => if (i == 15) BigInt(1) else BigInt(0)),
          b = Seq.tabulate(N)(i => if (i == 2) BigInt(1) else BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 8: a15 * b15
        //
        // x^15 * x^15 = x^30
        // x^30 = x^(16 + 14) = -x^14
        //
        // expected:
        //   c[14] = -1 mod 2^24 = 0xffffff
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a15_b15_wrap_to_c14_negative",
          a = Seq.tabulate(N)(i => if (i == 15) BigInt(1) else BigInt(0)),
          b = Seq.tabulate(N)(i => if (i == 15) BigInt(1) else BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 9: 低 4 项内部乘法
        //
        // 这个 case 主要看最低 4 项局部 Toom-Cook 是否正常。
        //
        // a = [1, 2, 3, 4, 0, ..., 0]
        // b = [5, 6, 7, 8, 0, ..., 0]
        //
        // 因为最高次数只有 6，小于 16，所以不会发生 16 阶回绕。
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "low4_manual_no_16_wrap",
          a = Seq(1, 2, 3, 4).map(BigInt(_)) ++ Seq.fill(12)(BigInt(0)),
          b = Seq(5, 6, 7, 8).map(BigInt(_)) ++ Seq.fill(12)(BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 10: 跨 4 项分块但不触发 16 阶回绕
        //
        // 这个 case 能检查 Core16TC43 中：
        //
        //   EvalLayerTC43 -> Product4TC43 -> InterpLayerTC43
        //
        // 的跨分块连接是否正确。
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "cross_block_no_16_wrap",
          a = Seq.tabulate(N)(i => BigInt(i + 1)),
          b = Seq.tabulate(N)(i => BigInt((i % 5) + 1)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 11: 回绕抵消 case
        //
        // a = 1 + x^15
        // b = 1 + x
        //
        // 普通乘法：
        //   (1 + x^15)(1 + x)
        // = 1 + x + x^15 + x^16
        //
        // 因为 x^16 = -1：
        //   1 + x + x^15 - 1 = x + x^15
        //
        // expected:
        //   c[1]  = 1
        //   c[15] = 1
        //   c[0]  = 0
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "wrap_cancellation_16",
          a = Seq.tabulate(N)(i => if (i == 0 || i == 15) BigInt(1) else BigInt(0)),
          b = Seq.tabulate(N)(i => if (i == 0 || i == 1) BigInt(1) else BigInt(0)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 12: 全 1 case
        //
        // 用于检查大量累加和负回绕。
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "all_ones",
          a = Seq.fill(N)(BigInt(1)),
          b = Seq.fill(N)(BigInt(1)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 13: 小随机。
        // ---------------------------------------------------------------------
        val rngSmall = new Random(7)

        for (caseId <- 0 until 20) {
          val a = Seq.fill(N)(BigInt(rngSmall.nextInt(16)))
          val b = Seq.fill(N)(BigInt(rngSmall.nextInt(16)))

          runCase(
            dut = dut,
            label = s"small_random_$caseId",
            a = a,
            b = b,
            printAll = false
          )
        }

        // ---------------------------------------------------------------------
        // Case 14: 接近真实输入范围的随机。
        //
        // a 为 AWidth bit，b 为 BWidth bit。
        // 默认 AWidth = 24，BWidth = 8。
        // ---------------------------------------------------------------------
        val rngFull = new Random(42)

        for (caseId <- 0 until 50) {
          val a = Seq.fill(N)(BigInt(AWidth, rngFull))
          val b = Seq.fill(N)(BigInt(BWidth, rngFull))

          runCase(
            dut = dut,
            label = s"full_random_$caseId",
            a = a,
            b = b,
            printAll = false
          )
        }

        // ---------------------------------------------------------------------
        // Case 15: 最大值边界测试。
        //
        // 这个 case 用于检查：
        //   1. 输入全接近最大值；
        //   2. 内部乘法累加是否有截断问题；
        //   3. 最终低 24 位是否与参考模型一致。
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "max_value_boundary",
          a = Seq.fill(N)(MaskA),
          b = Seq.fill(N)(MaskB),
          printAll = true
        )
      }
  }
}