package core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

object Core16TestParams {
  val AWidth = 24
  val BWidth = 8

  def dut: core16 = new core16(aWidth = AWidth, bWidth = BWidth)
}

class core16Test extends AnyFlatSpec with ChiselScalatestTester {
  private val N = 16
  private val QMask: BigInt = (BigInt(1) << Core16TestParams.AWidth) - 1
  private val BMask: BigInt = (BigInt(1) << Core16TestParams.BWidth) - 1

  private def schoolbookNegacyclic(a: Seq[BigInt], b: Seq[BigInt]): Seq[BigInt] = {
    require(a.length == N)
    require(b.length == N)

    val c = Array.fill(N)(BigInt(0))
    for (i <- 0 until N) {
      for (j <- i until N) {
        c(j) = c(j) + a(i) * b(j - i)
      }
      for (j <- 0 until i) {
        c(j) = c(j) - a(i) * b(N + j - i)
      }
    }

    c.map(_ & QMask).toSeq
  }

  private def runCase(
      dut: core16,
      label: String,
      aVals: Seq[BigInt],
      bVals: Seq[BigInt]
  ): Unit = {
    val expected = schoolbookNegacyclic(aVals, bVals)

    for (i <- 0 until N) {
      dut.io.a(i).poke((aVals(i) & QMask).U)
      dut.io.b(i).poke((bVals(i) & BMask).U)
    }

    dut.io.valid_in.poke(true.B)
    dut.clock.step(1)
    dut.io.valid_in.poke(false.B)

    var cycle = 0
    while (!dut.io.valid_out.peek().litToBoolean && cycle < 100) {
      dut.clock.step(1)
      cycle += 1
    }
    assert(dut.io.valid_out.peek().litToBoolean, s"[$label] valid_out timeout")

    var mismatches = 0
    for (i <- 0 until N) {
      val got = dut.io.c(i).peek().litValue & QMask
      val exp = expected(i) & QMask
      if (got != exp) {
        mismatches += 1
        if (mismatches <= 10) {
          println(
            s"[$label mismatch] c[$i] got=0x${got.toString(16)} expected=0x${exp.toString(16)}"
          )
        }
      }
    }

    assert(mismatches == 0, s"[$label] $mismatches coefficients mismatched")
    dut.clock.step(2)
  }

  behavior of "core16"

  it should "match the negacyclic schoolbook reference" in {
    test(Core16TestParams.dut) { dut =>
      dut.clock.setTimeout(0)
      dut.io.valid_in.poke(false.B)

      runCase(dut, "all_zero", Seq.fill(N)(BigInt(0)), Seq.fill(N)(BigInt(0)))
      runCase(dut, "all_one", Seq.fill(N)(BigInt(1)), Seq.fill(N)(BigInt(1)))
      runCase(
        dut,
        "alternating",
        Seq.tabulate(N)(i => if (i % 2 == 0) QMask else BigInt(0)),
        Seq.tabulate(N)(i => if (i % 2 == 0) BMask else BigInt(0))
      )
      runCase(dut, "maximum", Seq.fill(N)(QMask), Seq.fill(N)(BMask))

      val rand = new Random(16)
      for (trial <- 0 until 10) {
        val a = Seq.fill(N)(BigInt(Core16TestParams.AWidth, rand))
        val b = Seq.fill(N)(BigInt(Core16TestParams.BWidth, rand))
        runCase(dut, s"random_$trial", a, b)
      }
    }
  }
}
