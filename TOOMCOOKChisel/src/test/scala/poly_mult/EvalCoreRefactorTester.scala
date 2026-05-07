package poly_mult_sram

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class EvalPointVec1024EquivHarness(memW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(1024, UInt(memW.W)))
    val pt0 = Input(UInt(3.W))
    val pt1 = Input(UInt(3.W))
    val pt2 = Input(UInt(3.W))
    val phase = Input(UInt(2.W))
    val newOut = Output(Vec(16, UInt(outW.W)))
    val oldOut = Output(Vec(4, UInt(outW.W)))
  })

  val evalVec = Module(new EvalPointVec1024(memW, outW))
  evalVec.io.in := io.in
  evalVec.io.pt0 := io.pt0
  evalVec.io.pt1 := io.pt1
  evalVec.io.pt2 := io.pt2
  io.newOut := evalVec.io.out

  val lanes = (0 until 4).map(l => Module(new EvalLaneFixed(memW, outW, l, 4)))
  for (l <- 0 until 4) {
    lanes(l).io.in := io.in
    lanes(l).io.pt0 := io.pt0
    lanes(l).io.pt1 := io.pt1
    lanes(l).io.pt2 := io.pt2
    lanes(l).io.phase := io.phase
    io.oldOut(l) := lanes(l).io.out
  }
}

class EvalCoreRefactorTester extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "EvalPointVec1024"

  it should "match EvalLaneFixed address mapping for every point" in {
    test(new EvalPointVec1024EquivHarness(8, TC4EvalWidth.B_EVAL_W)) { dut =>
      val rng = new Random(1234)
      val in = Seq.fill(1024)(BigInt(rng.nextInt(256)))
      for (i <- 0 until 1024) dut.io.in(i).poke(in(i).U)

      for (pt0 <- 0 until 7; pt1 <- 0 until 7; pt2 <- 0 until 7; phase <- 0 until 4) {
        dut.io.pt0.poke(pt0.U)
        dut.io.pt1.poke(pt1.U)
        dut.io.pt2.poke(pt2.U)
        dut.io.phase.poke(phase.U)
        for (lane <- 0 until 4) {
          val idx = phase * 4 + lane
          dut.io.oldOut(lane).expect(dut.io.newOut(idx).peek(), s"pt=($pt0,$pt1,$pt2) phase=$phase lane=$lane idx=$idx")
        }
      }
    }
  }

  behavior of "EvalCorePingPongSram"

  private def pokeJob(dut: EvalCorePingPongSram, buf: Int, addr: Int, aW: Int, bW: Int): Unit = {
    dut.io.wrEn.poke(true.B)
    dut.io.wrBuf.poke(buf.U)
    dut.io.wrAddr.poke(addr.U)
    dut.io.wrGroup.poke((addr / 7).U)
    dut.io.wrPt2.poke((addr % 7).U)
    for (i <- 0 until 16) {
      dut.io.wrAvec(i).poke(((buf << 12) + (addr << 4) + i).U)
      dut.io.wrBvec(i).poke(((buf << 11) + (addr << 2) + (i & 3)).U)
    }
  }

  private def expectReadJob(dut: EvalCorePingPongSram, buf: Int, addr: Int): Unit = {
    dut.io.rdValid.expect(true.B)
    dut.io.rdGroup.expect((addr / 7).U)
    dut.io.rdPt2.expect((addr % 7).U)
    for (i <- 0 until 16) {
      dut.io.rdAvec(i).expect(((buf << 12) + (addr << 4) + i).U)
      dut.io.rdBvec(i).expect(((buf << 11) + (addr << 2) + (i & 3)).U)
    }
  }

  private def readJob(dut: EvalCorePingPongSram, buf: Int, addr: Int): Unit = {
    dut.io.wrEn.poke(false.B)
    dut.io.rdEn.poke(true.B)
    dut.io.rdBuf.poke(buf.U)
    dut.io.rdAddr.poke(addr.U)
    dut.clock.step(1)
    dut.io.rdEn.poke(false.B)
    expectReadJob(dut, buf, addr)
  }

  it should "store and replay all jobs from both ping-pong buffers" in {
    test(new EvalCorePingPongSram(16, 12)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      dut.io.rdEn.poke(false.B)
      dut.io.wrEn.poke(false.B)

      for (buf <- 0 until 2) {
        for (addr <- 0 until 343) {
          pokeJob(dut, buf, addr, 16, 12)
          dut.clock.step(1)
        }
        dut.io.wrEn.poke(false.B)
        for (addr <- 0 until 343) readJob(dut, buf, addr)
      }
    }
  }

  it should "allow simultaneous write and read to different buffers" in {
    test(new EvalCorePingPongSram(16, 12)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      // Seed buffer 0 address 0.
      pokeJob(dut, 0, 0, 16, 12)
      dut.io.rdEn.poke(false.B)
      dut.clock.step(1)

      // Read buffer 0 while writing buffer 1; the assertion permits wrBuf =/= rdBuf.
      pokeJob(dut, 1, 0, 16, 12)
      dut.io.rdEn.poke(true.B)
      dut.io.rdBuf.poke(0.U)
      dut.io.rdAddr.poke(0.U)
      dut.clock.step(1)
      dut.io.wrEn.poke(false.B)
      dut.io.rdEn.poke(false.B)
      expectReadJob(dut, 0, 0)

      readJob(dut, 1, 0)
    }
  }
}
