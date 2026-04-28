package poly_mult

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class Product4Test extends AnyFlatSpec with ChiselScalatestTester {

  private val A_EVAL_W = 39
  private val B_EVAL_W = 29
  private val OUT_W    = 36

  private val MaskA: BigInt   = (BigInt(1) << A_EVAL_W) - 1
  private val MaskB: BigInt   = (BigInt(1) << B_EVAL_W) - 1
  private val MaskOut: BigInt = (BigInt(1) << OUT_W) - 1

  private def hex(x: BigInt): String = "0x" + x.toString(16)

  private def modOut(x: BigInt): BigInt = x & MaskOut

  /**
    * 当前 Product4 的有效语义：
    * 4 项输入经过 Product4 后输出 4 项负循环结果，out(4..6)=0。
    *
    * 环：
    *   x^4 + 1 = 0
    * 即：
    *   x^4 = -1
    *   x^5 = -x
    *   x^6 = -x^2
    */
  private def schoolbook4Negacyclic(
      a: Seq[BigInt],
      b: Seq[BigInt]
  ): Seq[BigInt] = {
    require(a.length == 4)
    require(b.length == 4)

    val d = Array.fill(7)(BigInt(0))

    for (i <- 0 until 4) {
      for (j <- 0 until 4) {
        d(i + j) += a(i) * b(j)
      }
    }

    val c0 = d(0) - d(4)
    val c1 = d(1) - d(5)
    val c2 = d(2) - d(6)
    val c3 = d(3)

    Seq(c0, c1, c2, c3, BigInt(0), BigInt(0), BigInt(0)).map(modOut)
  }

  private def pokeProductInput(
      dut: Product4,
      a: Seq[BigInt],
      b: Seq[BigInt]
  ): Unit = {
    require(a.length == 4)
    require(b.length == 4)

    for (i <- 0 until 4) {
      dut.io.a4(i).poke((a(i) & MaskA).U)
      dut.io.b4(i).poke((b(i) & MaskB).U)
    }
  }

  private def runProductCase(
      dut: Product4,
      label: String,
      a: Seq[BigInt],
      b: Seq[BigInt],
      printAll: Boolean = true
  ): Unit = {
    println(s"========== Product4 case: $label ==========")

    val expected = schoolbook4Negacyclic(a, b)

    pokeProductInput(dut, a, b)

    // Product4 是组合模块；step 1 拍只是让波形/打印更稳定。
    dut.clock.step(1)

    var mismatchCount = 0

    for (i <- 0 until 7) {
      val raw = dut.io.out(i).peek().litValue
      val got = raw & MaskOut
      val exp = expected(i)

      if (printAll) {
        println(
          s"[$label] out[$i] raw36=${hex(raw)} masked36=${hex(got)} expected36=${hex(exp)}"
        )
      }

      if (got != exp) {
        mismatchCount += 1
        println(
          s"[$label MISMATCH] out[$i] raw36=${hex(raw)} masked36=${hex(got)} expected36=${hex(exp)}"
        )
      }
    }

    assert(
      mismatchCount == 0,
      s"[$label] Product4 mismatchCount=$mismatchCount"
    )

    println(s"[$label] PASS")
  }

  behavior of "Product4"

  it should "match 4-term negacyclic reference for deterministic and random cases" in {
    test(new Product4)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

        dut.clock.setTimeout(0)

        runProductCase(
          dut,
          label = "zero",
          a = Seq(0, 0, 0, 0).map(BigInt(_)),
          b = Seq(0, 0, 0, 0).map(BigInt(_))
        )

        runProductCase(
          dut,
          label = "a0_b0",
          a = Seq(1, 0, 0, 0).map(BigInt(_)),
          b = Seq(1, 0, 0, 0).map(BigInt(_))
        )

        runProductCase(
          dut,
          label = "a1_b0",
          a = Seq(0, 1, 0, 0).map(BigInt(_)),
          b = Seq(1, 0, 0, 0).map(BigInt(_))
        )

        runProductCase(
          dut,
          label = "a0_b1",
          a = Seq(1, 0, 0, 0).map(BigInt(_)),
          b = Seq(0, 1, 0, 0).map(BigInt(_))
        )

        runProductCase(
          dut,
          label = "a3_b1_wrap",
          a = Seq(0, 0, 0, 1).map(BigInt(_)),
          b = Seq(0, 1, 0, 0).map(BigInt(_))
        )

        runProductCase(
          dut,
          label = "a0_b3",
          a = Seq(1, 0, 0, 0).map(BigInt(_)),
          b = Seq(0, 0, 0, 1).map(BigInt(_))
        )

        runProductCase(
          dut,
          label = "small_manual",
          a = Seq(1, 2, 3, 4).map(BigInt(_)),
          b = Seq(5, 6, 7, 8).map(BigInt(_))
        )

        val rng = new Random(7)
        for (caseId <- 0 until 20) {
          val a = Seq.fill(4)(BigInt(rng.nextInt(16)))
          val b = Seq.fill(4)(BigInt(rng.nextInt(16)))

          runProductCase(
            dut,
            label = s"small_random_$caseId",
            a = a,
            b = b,
            printAll = false
          )
        }
      }
  }
}