package poly_mult

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class Product4TC43Test extends AnyFlatSpec with ChiselScalatestTester {

  private val AWidth = 24
  private val BWidth = 8
  private val OutWidth = 36

  private val MaskA: BigInt = (BigInt(1) << AWidth) - 1
  private val MaskB: BigInt = (BigInt(1) << BWidth) - 1
  private val MaskOut: BigInt = (BigInt(1) << OutWidth) - 1

  private def schoolbook4(
      a: Seq[BigInt],
      b: Seq[BigInt]
  ): Seq[BigInt] = {
    require(a.length == 4, s"a length must be 4, got ${a.length}")
    require(b.length == 4, s"b length must be 4, got ${b.length}")

    val c = Array.fill(7)(BigInt(0))

    for (i <- 0 until 4) {
      for (j <- 0 until 4) {
        c(i + j) = c(i + j) + (a(i) & MaskA) * (b(j) & MaskB)
      }
    }

    c.map(_ & MaskOut).toSeq
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

    val expected = schoolbook4(a, b)

    println(s"========== Product4TC43 case: $label ==========")

    for (i <- 0 until 4) {
      dut.io.a4(i).poke((a(i) & MaskA).U)
      dut.io.b4(i).poke((b(i) & MaskB).U)
    }

    // Product4TC43 是组合模块，这里 step 1 拍只是为了让测试行为更稳定、日志更清楚。
    dut.clock.step(1)

    var mismatchCount = 0

    for (i <- 0 until 7) {
      val gotRaw = dut.io.out(i).peek().litValue
      val gotMasked = gotRaw & MaskOut
      val exp = expected(i) & MaskOut

      if (printAll) {
        println(
          s"[$label] out[$i] raw=${hex(gotRaw)} masked=${hex(gotMasked)} expected=${hex(exp)}"
        )
      }

      if (gotMasked != exp) {
        mismatchCount += 1
        println(
          s"[$label MISMATCH] out[$i] raw=${hex(gotRaw)} masked=${hex(gotMasked)} expected=${hex(exp)}"
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

  it should "match 4x4 schoolbook multiplication modulo 2^36" in {
    test(new Product4TC43)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

        dut.clock.setTimeout(0)

        // ---------------------------------------------------------------------
        // Case 0: 全 0
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
        // 理论 out[0] = 1
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a0_b0",
          a = Seq(1, 0, 0, 0).map(BigInt(_)),
          b = Seq(1, 0, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 2: a1 * b0
        // 理论 out[1] = 1
        // 你之前看到 raw=0xc00000001，masked 后应为 0x1。
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a1_b0",
          a = Seq(0, 1, 0, 0).map(BigInt(_)),
          b = Seq(1, 0, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 3: a0 * b1
        // 理论 out[1] = 1
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a0_b1",
          a = Seq(1, 0, 0, 0).map(BigInt(_)),
          b = Seq(0, 1, 0, 0).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 4: a3 * b3
        // 理论 out[6] = 1
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "a3_b3",
          a = Seq(0, 0, 0, 1).map(BigInt(_)),
          b = Seq(0, 0, 0, 1).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 5: 小整数手算 case
        // a = [1,2,3,4], b = [5,6,7,8]
        // expected = [5,16,34,60,61,52,32]
        // ---------------------------------------------------------------------
        runCase(
          dut = dut,
          label = "small_manual",
          a = Seq(1, 2, 3, 4).map(BigInt(_)),
          b = Seq(5, 6, 7, 8).map(BigInt(_)),
          printAll = true
        )

        // ---------------------------------------------------------------------
        // Case 6: 小随机，多跑几组，避免只测单位脉冲。
        // ---------------------------------------------------------------------
        val rngSmall = new Random(7)
        for (caseId <- 0 until 10) {
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
        // Case 7: 接近真实输入范围的随机。
        // a 为 24 bit，b 为 8 bit。
        // ---------------------------------------------------------------------
        val rngFull = new Random(42)
        for (caseId <- 0 until 10) {
          val a = Seq.fill(4)(BigInt(rngFull.nextInt() & 0xffffff))
          val b = Seq.fill(4)(BigInt(rngFull.nextInt() & 0xff))

          runCase(
            dut = dut,
            label = s"full_random_$caseId",
            a = a,
            b = b,
            printAll = false
          )
        }
      }
  }
}