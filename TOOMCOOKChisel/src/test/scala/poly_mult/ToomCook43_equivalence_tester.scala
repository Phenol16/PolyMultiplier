package poly_mult_sram

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class ToomCook43EquivalenceHarness extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(1024, UInt(24.W)))
    val b = Input(Vec(1024, UInt(8.W)))
    val new_valid_out = Output(Bool())
    val legacy_valid_out = Output(Bool())
    val new_c = Output(Vec(1024, UInt(24.W)))
    val legacy_c = Output(Vec(1024, UInt(24.W)))
  })

  val modular = Module(new ToomCook43)
  val legacy = Module(new ToomCook43Legacy)

  modular.io.valid_in := io.valid_in
  legacy.io.valid_in := io.valid_in
  modular.io.a := io.a
  legacy.io.a := io.a
  modular.io.b := io.b
  legacy.io.b := io.b

  io.new_valid_out := modular.io.valid_out
  io.legacy_valid_out := legacy.io.valid_out
  io.new_c := modular.io.c
  io.legacy_c := legacy.io.c
}

class ToomCook43EquivalenceTest extends AnyFlatSpec with ChiselScalatestTester {
  private val N = 1024
  private val QMask: BigInt = (BigInt(1) << 24) - 1

  private def runEquivCase(
      dut: ToomCook43EquivalenceHarness,
      label: String,
      aVals: Seq[BigInt],
      bVals: Seq[BigInt],
      maxWaitCycles: Int = 8000
  ): Unit = {
    for (i <- 0 until N) {
      dut.io.a(i).poke((aVals(i) & QMask).U)
      dut.io.b(i).poke((bVals(i) & BigInt("ff", 16)).U)
    }

    dut.io.valid_in.poke(true.B)
    dut.clock.step(1)
    dut.io.valid_in.poke(false.B)

    var sawNew = false
    var sawLegacy = false
    val newOut = Array.fill(N)(BigInt(0))
    val legacyOut = Array.fill(N)(BigInt(0))
    var cycle = 0

    while ((!sawNew || !sawLegacy) && cycle < maxWaitCycles) {
      if (dut.io.new_valid_out.peek().litToBoolean) {
        sawNew = true
        for (i <- 0 until N) newOut(i) = dut.io.new_c(i).peek().litValue & QMask
      }
      if (dut.io.legacy_valid_out.peek().litToBoolean) {
        sawLegacy = true
        for (i <- 0 until N) legacyOut(i) = dut.io.legacy_c(i).peek().litValue & QMask
      }
      if (!sawNew || !sawLegacy) {
        dut.clock.step(1)
        cycle += 1
      }
    }

    assert(sawNew, s"[$label] modular ToomCook43 did not produce valid_out")
    assert(sawLegacy, s"[$label] legacy ToomCook43Legacy did not produce valid_out")
    val mismatches = newOut.indices.filter(i => newOut(i) != legacyOut(i)).take(20)
    assert(mismatches.isEmpty, s"[$label] modular output differs from legacy at ${mismatches.mkString(",")}")
    dut.clock.step(2)
  }

  behavior of "Modular ToomCook43 vs legacy ToomCook43Legacy"

  it should "match bit-for-bit on directed and randomized frames" in {
    test(new ToomCook43EquivalenceHarness)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.clock.setTimeout(0)

        runEquivCase(dut, "all_zeros", Seq.fill(N)(BigInt(0)), Seq.fill(N)(BigInt(0)))
        runEquivCase(dut, "all_ones", Seq.fill(N)(BigInt(1)), Seq.fill(N)(BigInt(1)))

        val sparseRng = new Random(17)
        val sparseA = Array.fill(N)(BigInt(0))
        val sparseB = Array.fill(N)(BigInt(0))
        for (_ <- 0 until 24) sparseA(sparseRng.nextInt(N)) = BigInt(sparseRng.nextInt() & 0xffffff)
        for (_ <- 0 until 24) sparseB(sparseRng.nextInt(N)) = BigInt(sparseRng.nextInt() & 0xff)
        runEquivCase(dut, "sparse_random", sparseA.toSeq, sparseB.toSeq)

        val denseRng = new Random(43)
        runEquivCase(
          dut,
          "dense_random",
          Seq.fill(N)(BigInt(denseRng.nextInt() & 0xffffff)),
          Seq.fill(N)(BigInt(denseRng.nextInt() & 0xff))
        )

        runEquivCase(
          dut,
          "edge_values",
          Seq.tabulate(N)(i => if ((i & 1) == 0) QMask else BigInt(0)),
          Seq.tabulate(N)(i => if ((i & 1) == 0) BigInt("ff", 16) else BigInt(0))
        )
      }
  }
}
