package poly_mult

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class Core16TC43Test extends AnyFlatSpec with ChiselScalatestTester {

  private val N = 16

  private val A_EVAL_W = 39
  private val B_EVAL_W = 29
  private val OUT_W    = 33

  private val MaskA: BigInt   = (BigInt(1) << A_EVAL_W) - 1
  private val MaskB: BigInt   = (BigInt(1) << B_EVAL_W) - 1
  private val MaskOut: BigInt = (BigInt(1) << OUT_W) - 1

  private def hex(x: BigInt): String = "0x" + x.toString(16)

  private def modOut(x: BigInt): BigInt = x & MaskOut

  private def zeroVec: Seq[BigInt] = Seq.fill(N)(BigInt(0))

  private def oneHot(pos: Int, value: BigInt = BigInt(1)): Seq[BigInt] = {
    require(pos >= 0 && pos < N)
    val arr = Array.fill(N)(BigInt(0))
    arr(pos) = value
    arr.toSeq
  }

  /**
    * 16 项负循环卷积：
    *   mod x^16 + 1
    *
    * if i+j < 16:
    *   c[i+j] += a[i]*b[j]
    * else:
    *   c[i+j-16] -= a[i]*b[j]
    */
  private def schoolbook16Negacyclic(
      a: Seq[BigInt],
      b: Seq[BigInt]
  ): Seq[BigInt] = {
    require(a.length == N)
    require(b.length == N)

    val c = Array.fill(N)(BigInt(0))

    for (i <- 0 until N) {
      for (j <- 0 until N) {
        val idx = i + j
        val prod = a(i) * b(j)

        if (idx < N) {
          c(idx) += prod
        } else {
          c(idx - N) -= prod
        }
      }
    }

    c.map(modOut).toSeq
  }

  private def pokeCoreInput(
      dut: Core16TC43,
      a: Seq[BigInt],
      b: Seq[BigInt]
  ): Unit = {
    require(a.length == N)
    require(b.length == N)

    for (i <- 0 until N) {
      dut.io.avec(i).poke((a(i) & MaskA).U)
      dut.io.bvec(i).poke((b(i) & MaskB).U)
    }
  }

  private def fireAndWaitCore(
      dut: Core16TC43,
      maxWait: Int = 20
  ): Int = {
    dut.io.valid_in.poke(true.B)
    dut.clock.step(1)
    dut.io.valid_in.poke(false.B)

    var cycle = 0
    var seen = dut.io.valid_out.peek().litToBoolean

    while (!seen && cycle < maxWait) {
      dut.clock.step(1)
      cycle += 1
      seen = dut.io.valid_out.peek().litToBoolean
    }

    assert(seen, s"Core16TC43 valid_out timeout, maxWait=$maxWait")
    cycle
  }

  private def runCoreCase(
      dut: Core16TC43,
      label: String,
      a: Seq[BigInt],
      b: Seq[BigInt],
      printAll: Boolean = true
  ): Unit = {
    println(s"========== Core16TC43 case: $label ==========")

    val expected = schoolbook16Negacyclic(a, b)

    pokeCoreInput(dut, a, b)

    val outCycle = fireAndWaitCore(dut)

    println(s"[$label] valid_out seen after $outCycle extra cycles")

    var mismatchCount = 0
    val nonZero = scala.collection.mutable.ArrayBuffer[(Int, BigInt)]()

    for (i <- 0 until N) {
      val raw = dut.io.cOut(i).peek().litValue
      val got = raw & MaskOut
      val exp = expected(i)

      if (got != 0) {
        nonZero += ((i, got))
      }

      if (printAll) {
        println(
          s"[$label] cOut[$i] raw36=${hex(raw)} masked36=${hex(got)} expected36=${hex(exp)}"
        )
      }

      if (got != exp) {
        mismatchCount += 1
        println(
          s"[$label MISMATCH] cOut[$i] raw36=${hex(raw)} masked36=${hex(got)} expected36=${hex(exp)}"
        )
      }
    }

    println(s"[$label] nonzero output count=${nonZero.length}")
    nonZero.take(20).foreach { case (idx, value) =>
      println(s"[$label NONZERO] cOut[$idx] = ${hex(value)}")
    }

    assert(
      mismatchCount == 0,
      s"[$label] Core16TC43 mismatchCount=$mismatchCount"
    )

    println(s"[$label] PASS")

    // 留 2 拍，避免下一个 case 和当前 valid_out/寄存器状态混在一起。
    dut.clock.step(2)
  }

  behavior of "Core16TC43"

  it should "match 16-term negacyclic schoolbook multiplication modulo x^16 + 1" in {
    test(new Core16TC43)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

        dut.clock.setTimeout(0)

        runCoreCase(
          dut,
          label = "zero",
          a = zeroVec,
          b = zeroVec
        )

        runCoreCase(
          dut,
          label = "a0_b0",
          a = oneHot(0),
          b = oneHot(0)
        )

        runCoreCase(
          dut,
          label = "a1_b0_no_wrap",
          a = oneHot(1),
          b = oneHot(0)
        )

        runCoreCase(
          dut,
          label = "a0_b1_no_wrap",
          a = oneHot(0),
          b = oneHot(1)
        )

        runCoreCase(
          dut,
          label = "a15_b0_no_wrap",
          a = oneHot(15),
          b = oneHot(0)
        )

        runCoreCase(
          dut,
          label = "a15_b1_wrap_neg",
          a = oneHot(15),
          b = oneHot(1)
        )

        runCoreCase(
          dut,
          label = "a8_b8_wrap_neg",
          a = oneHot(8),
          b = oneHot(8)
        )

        runCoreCase(
          dut,
          label = "small_manual",
          a = (0 until N).map(i => BigInt(i + 1)),
          b = (0 until N).map(i => BigInt((i % 5) + 1)),
          printAll = false
        )

        val rng = new Random(11)
        for (caseId <- 0 until 20) {
          val a = Seq.fill(N)(BigInt(rng.nextInt(16)))
          val b = Seq.fill(N)(BigInt(rng.nextInt(16)))

          runCoreCase(
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