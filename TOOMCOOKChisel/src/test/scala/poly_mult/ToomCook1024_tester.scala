package poly_mult

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import java.time.LocalTime

class ToomCook1024Test extends AnyFlatSpec with ChiselScalatestTester {

  // negacyclic schoolbook:
  // c[j] += a[i] * b[j - i]      when j >= i
  // c[j] -= a[i] * b[n + j - i]  when j < i
  // result mod 2^24
  private def schoolbook(a: Seq[BigInt], b: Seq[BigInt], n: Int): Seq[BigInt] = {
    val c = Array.fill(n)(BigInt(0))
    val qMask = (BigInt(1) << 24) - 1

    for (i <- 0 until n) {
      for (j <- i until n) {
        c(j) = c(j) + a(i) * b(j - i)
      }
      for (j <- 0 until i) {
        c(j) = c(j) - a(i) * b(n + j - i)
      }
    }

    c.map(x => x & qMask).toSeq
  }

  private def now(): String = LocalTime.now().toString

  behavior of "ToomCook1024"

  it should "work for one deterministic random case" in {
    println(s"[${now()}] before test(new ToomCook43)")

    test(new ToomCook43) { dut =>
      println(s"[${now()}] after elaboration, simulation started")

      val n = 1024
      val maxWaitCycles = 3000

      val rng = new Random(42)

      val aVals: Seq[BigInt] =
        Seq.fill(n)(BigInt(rng.nextInt() & 0xffffff))

      val bVals: Seq[BigInt] =
        Seq.fill(n)(BigInt(rng.nextInt() & 0xff))

      println(s"[${now()}] start software schoolbook reference")
      val cExpected = schoolbook(aVals, bVals, n)
      println(s"[${now()}] finish software schoolbook reference")

      // 初始稳定几拍
      dut.io.valid_in.poke(false.B)
      dut.clock.step(5)

      println(s"[${now()}] start poking inputs")
      for (i <- 0 until n) {
        dut.io.a(i).poke(aVals(i).U)
        dut.io.b(i).poke(bVals(i).U)
      }
      println(s"[${now()}] finish poking inputs")

      // 输入 valid 拉高 1 拍
      println(s"[${now()}] send valid_in")
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      // 等待 valid_out，不能无限等
      println(s"[${now()}] start waiting valid_out")

      var seenValid = false
      var outCycle = -1

      for (cycle <- 0 until maxWaitCycles) {
        val v = dut.io.valid_out.peek().litToBoolean

        if (cycle % 100 == 0) {
          println(s"[${now()}] cycle=$cycle valid_out=$v")
        }

        if (v && !seenValid) {
          seenValid = true
          outCycle = cycle
          println(s"[${now()}] valid_out asserted at cycle=$cycle")
        }

        if (!seenValid) {
          dut.clock.step(1)
        }
      }

      assert(
        seenValid,
        s"Error: 等待 io.valid_out 超时！maxWaitCycles=$maxWaitCycles"
      )

      println(s"[${now()}] start checking outputs, outCycle=$outCycle")

      var mismatchCount = 0
      val maxPrintMismatch = 20

      for (i <- 0 until n) {
        val got = dut.io.c(i).peek().litValue
        val exp = cExpected(i)

        if (got != exp) {
          mismatchCount += 1
          if (mismatchCount <= maxPrintMismatch) {
            println(
              s"[MISMATCH] index=$i got=0x${got.toString(16)} expected=0x${exp.toString(16)}"
            )
          }
        }
      }

      assert(
        mismatchCount == 0,
        s"计算错误：共有 $mismatchCount 个系数不匹配，最多已打印 $maxPrintMismatch 个。"
      )

      println(s"[${now()}] test passed, valid_out cycle=$outCycle")
    }

    println(s"[${now()}] test finished")
  }
}