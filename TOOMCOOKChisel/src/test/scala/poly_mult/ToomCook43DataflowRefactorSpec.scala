package poly_mult_sram

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class EvalBlock16TC4EquivHarness(memW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(1024, UInt(memW.W)))
    val pt0 = Input(UInt(3.W))
    val pt1 = Input(UInt(3.W))
    val pt2 = Input(UInt(3.W))
    val outNew = Output(Vec(16, UInt(outW.W)))
    val outOld = Output(Vec(16, UInt(outW.W)))
  })

  val block = Module(new EvalBlock16TC4(memW, outW))
  block.io.in := io.in
  block.io.pt0 := io.pt0
  block.io.pt1 := io.pt1
  block.io.pt2 := io.pt2
  io.outNew := block.io.out

  for (l <- 0 until 16) {
    val lane = Module(new EvalLaneFixed(memW, outW, laneConst = l % 4, evalLanes = 4))
    lane.io.in := io.in
    lane.io.pt0 := io.pt0
    lane.io.pt1 := io.pt1
    lane.io.pt2 := io.pt2
    lane.io.phase := (l / 4).U
    io.outOld(l) := lane.io.out
  }
}

class Core16TC4PipelineHarness extends Module {
  val io = IO(new Bundle {
    val validIn = Input(Bool())
    val tagIn = Input(UInt(8.W))
    val validOut = Output(Bool())
    val tagOut = Output(UInt(8.W))
  })

  val core = Module(new Core16TC4)
  core.io.valid_in := io.validIn
  for (i <- 0 until 16) {
    core.io.avec(i) := (io.tagIn + i.U)(TC4EvalWidth.A_EVAL_W - 1, 0)
    core.io.bvec(i) := (io.tagIn + (2 * i).U)(TC4EvalWidth.B_EVAL_W - 1, 0)
  }

  val tagPipe = RegEnable(io.tagIn, io.validIn)
  io.validOut := core.io.valid_out
  io.tagOut := tagPipe
}

class ToomCook43EquivalenceHarness extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(1024, UInt(24.W)))
    val b = Input(Vec(1024, UInt(8.W)))
    val baseline_valid_out = Output(Bool())
    val new_valid_out = Output(Bool())
    val baseline_c = Output(Vec(1024, UInt(24.W)))
    val new_c = Output(Vec(1024, UInt(24.W)))
  })

  val baseline = Module(new ToomCook43Baseline)
  val dataflow = Module(new ToomCook43)

  baseline.io.valid_in := io.valid_in
  dataflow.io.valid_in := io.valid_in
  baseline.io.a := io.a
  dataflow.io.a := io.a
  baseline.io.b := io.b
  dataflow.io.b := io.b

  io.baseline_valid_out := baseline.io.valid_out
  io.new_valid_out := dataflow.io.valid_out
  io.baseline_c := baseline.io.c
  io.new_c := dataflow.io.c
}

class EvalBlock16TC4Spec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "EvalBlock16TC4"

  private def runEquivalence(memW: Int, outW: Int): Unit = {
    test(new EvalBlock16TC4EquivHarness(memW, outW)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val rnd = new Random(0x4343 + memW + outW)
      val mask = (BigInt(1) << memW) - 1
      val values = Seq.fill(1024)(BigInt(memW, rnd) & mask)
      for (i <- 0 until 1024) dut.io.in(i).poke(values(i).U)

      for (pt0 <- 0 until 7; pt1 <- 0 until 7; pt2 <- 0 until 7) {
        dut.io.pt0.poke(pt0.U)
        dut.io.pt1.poke(pt1.U)
        dut.io.pt2.poke(pt2.U)
        dut.clock.step(1)
        for (l <- 0 until 16) {
          dut.io.outNew(l).expect(dut.io.outOld(l).peek(), s"memW=$memW outW=$outW pt=($pt0,$pt1,$pt2) lane=$l")
        }
      }
    }
  }

  it should "match EvalLaneFixed for A width" in {
    runEquivalence(memW = 24, outW = TC4EvalWidth.A_EVAL_W)
  }

  it should "match EvalLaneFixed for B width" in {
    runEquivalence(memW = 8, outW = TC4EvalWidth.B_EVAL_W)
  }
}

class Core16TC4PipelineSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Core16TC4 pipeline scheduling"

  it should "allow consecutive valid_in pulses and align one-cycle metadata" in {
    test(new Core16TC4PipelineHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.io.validIn.poke(false.B)
      dut.clock.step(2)

      val n = 16
      for (cycle <- 0 until n) {
        dut.io.validIn.poke(true.B)
        dut.io.tagIn.poke(cycle.U)
        dut.clock.step(1)
        if (cycle > 0) {
          dut.io.validOut.expect(true.B)
          dut.io.tagOut.expect((cycle - 1).U)
        }
      }
      dut.io.validIn.poke(false.B)
      dut.clock.step(1)
      dut.io.validOut.expect(true.B)
      dut.io.tagOut.expect((n - 1).U)
      dut.clock.step(1)
      dut.io.validOut.expect(false.B)
    }
  }
}

class ToomCook43EquivalenceSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ToomCook43 dataflow top"

  private val n = 1024
  private val q24 = (BigInt(1) << 24) - 1
  private val q8 = (BigInt(1) << 8) - 1

  private def pokeFrame(dut: ToomCook43EquivalenceHarness, a: Seq[BigInt], b: Seq[BigInt]): Unit = {
    for (i <- 0 until n) {
      dut.io.a(i).poke((a(i) & q24).U)
      dut.io.b(i).poke((b(i) & q8).U)
    }
    dut.io.valid_in.poke(true.B)
    dut.clock.step(1)
    dut.io.valid_in.poke(false.B)
  }

  private def waitForBoth(dut: ToomCook43EquivalenceHarness, maxCycles: Int): Unit = {
    var baselineSeen = false
    var newSeen = false
    val baselineOut = Array.fill(n)(BigInt(0))
    val newOut = Array.fill(n)(BigInt(0))
    var cycle = 0
    while ((!baselineSeen || !newSeen) && cycle < maxCycles) {
      if (dut.io.baseline_valid_out.peek().litToBoolean && !baselineSeen) {
        baselineSeen = true
        for (i <- 0 until n) baselineOut(i) = dut.io.baseline_c(i).peek().litValue & q24
      }
      if (dut.io.new_valid_out.peek().litToBoolean && !newSeen) {
        newSeen = true
        for (i <- 0 until n) newOut(i) = dut.io.new_c(i).peek().litValue & q24
      }
      if (!baselineSeen || !newSeen) {
        dut.clock.step(1)
        cycle += 1
      }
    }
    assert(baselineSeen, s"baseline did not assert valid_out within $maxCycles cycles")
    assert(newSeen, s"new dataflow design did not assert valid_out within $maxCycles cycles")
    for (i <- 0 until n) assert(baselineOut(i) == newOut(i), s"c($i) mismatch baseline=${baselineOut(i)} new=${newOut(i)}")
  }

  it should "match ToomCook43Baseline for deterministic and random frames" in {
    test(new ToomCook43EquivalenceHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      val rnd = new Random(43)
      val cases = Seq(
        "zero" -> (Seq.fill(n)(BigInt(0)), Seq.fill(n)(BigInt(0))),
        "ones" -> (Seq.fill(n)(BigInt(1)), Seq.fill(n)(BigInt(1))),
        "max" -> (Seq.fill(n)(q24), Seq.fill(n)(q8)),
        "sparse" -> ((0 until n).map(i => if (i % 97 == 0) BigInt(i + 1) else BigInt(0)), (0 until n).map(i => if (i % 113 == 0) BigInt(255 - (i % 17)) else BigInt(0))),
        "random" -> (Seq.fill(n)(BigInt(24, rnd)), Seq.fill(n)(BigInt(8, rnd)))
      )

      for ((_, (a, b)) <- cases) {
        pokeFrame(dut, a, b)
        waitForBoth(dut, maxCycles = 12000)
        dut.clock.step(5)
      }
    }
  }
}

class ToomCook43LatencySpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ToomCook43 latency and debug counters"

  it should "report expected job/block counts for one frame" in {
    test(new ToomCook43).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      for (i <- 0 until 1024) {
        dut.io.a(i).poke((BigInt(i) & ((BigInt(1) << 24) - 1)).U)
        dut.io.b(i).poke((BigInt(i * 3 + 1) & 255).U)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      var cycle = 0
      while (!dut.io.valid_out.peek().litToBoolean && cycle < 12000) {
        dut.clock.step(1)
        cycle += 1
      }
      assert(cycle < 12000, "valid_out timeout")
      dut.evalJobsIssued.expect(343.U)
      dut.coreJobsAccepted.expect(343.U)
      dut.coreJobsCompleted.expect(343.U)
      dut.w2BlocksCompleted.expect(49.U)
      dut.w1BlocksCompleted.expect(7.U)
      dut.w0BlocksCompleted.expect(7.U)
      println(s"ToomCook43 latency cycles from valid_in deassertion wait loop = $cycle")
    }
  }
}

class W2BankPoolBehaviorSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "W2 bank pool integration"

  it should "complete 49 W2 descriptors without dropping any of the 343 core jobs" in {
    test(new ToomCook43).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      for (i <- 0 until 1024) {
        dut.io.a(i).poke((if (i == 0) BigInt(1) else BigInt(0)).U)
        dut.io.b(i).poke((if (i == 0) BigInt(1) else BigInt(0)).U)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      var cycle = 0
      while (dut.w2BlocksCompleted.peek().litValue < 49 && cycle < 6000) {
        dut.clock.step(1)
        cycle += 1
      }
      dut.evalJobsIssued.expect(343.U)
      dut.coreJobsAccepted.expect(343.U)
      dut.coreJobsCompleted.expect(343.U)
      dut.w2BlocksCompleted.expect(49.U)
    }
  }
}
