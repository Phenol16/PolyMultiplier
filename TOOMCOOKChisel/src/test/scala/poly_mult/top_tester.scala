package poly_mult

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class TopTest extends AnyFlatSpec with ChiselScalatestTester {
  private def schoolbook(a: Seq[Long], b: Seq[Long], n: Int): Seq[Long] = {
    val c = Array.fill(n)(0L)
    for (i <- 0 until n) {
      for (j <- i until n) {
        c(j) += a(i) * b(j - i)
      }
      for (j <- 0 until i) {
        c(j) -= a(i) * b(n + j - i)
      }
    }
    c.map(_ & 0xffffffL).toSeq
  }

  private def driveInput(dut: top, aVals: Seq[Long], bVals: Seq[Long]): Unit = {
    for (i <- 0 until 1024) {
      dut.io.a(i).poke(aVals(i).U(24.W))
      dut.io.b(i).poke(bVals(i).U(8.W))
    }
    dut.io.valid_in.poke(true.B)
    dut.clock.step(1)
    dut.io.valid_in.poke(false.B)
  }

  private def waitValid(dut: top, maxCycles: Int): Unit = {
    var cycles = 0
    while (!dut.io.valid_out.peek().litToBoolean && cycles < maxCycles) {
      dut.clock.step(1)
      cycles += 1
    }
    assert(cycles < maxCycles, s"timeout: valid_out did not assert within $maxCycles cycles")
  }

  "top" should "match schoolbook result and support multiple requests" in {
    test(new top) { dut =>
      val n = 1024
      val rng = new Random(123)

      val aVals0 = Seq.fill(n)((rng.nextInt() & 0xffffff).toLong)
      val bVals0 = Seq.fill(n)((rng.nextInt() & 0xff).toLong)
      val expect0 = schoolbook(aVals0, bVals0, n)
      driveInput(dut, aVals0, bVals0)
      waitValid(dut, 120000)
      for (i <- 0 until n) {
        dut.io.c(i).expect(expect0(i).U, s"first transaction mismatch at index $i")
      }

      val aVals1 = Seq.fill(n)((rng.nextInt() & 0xffffff).toLong)
      val bVals1 = Seq.fill(n)((rng.nextInt() & 0xff).toLong)
      val expect1 = schoolbook(aVals1, bVals1, n)
      driveInput(dut, aVals1, bVals1)
      waitValid(dut, 120000)
      for (i <- 0 until n) {
        dut.io.c(i).expect(expect1(i).U, s"second transaction mismatch at index $i")
      }
    }
  }
}
