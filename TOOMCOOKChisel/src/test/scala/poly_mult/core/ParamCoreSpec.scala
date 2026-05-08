package poly_mult

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class ParamCoreSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def mask(width: Int): BigInt = (BigInt(1) << width) - 1
  private def wrap(v: BigInt, width: Int): BigInt = v & mask(width)

  def schoolbookNegacyclic(a: Seq[BigInt], b: Seq[BigInt], n: Int, outW: Int): Seq[BigInt] = {
    val c = Array.fill[BigInt](n)(BigInt(0))
    for (i <- 0 until n; j <- 0 until n) {
      val k = i + j
      val prod = a(i) * b(j)
      if (k >= n) c(k - n) -= prod else c(k) += prod
    }
    c.map(wrap(_, outW)).toSeq
  }

  private def randomVec(n: Int, width: Int, rng: Random): Seq[BigInt] =
    Seq.fill(n)(BigInt(width, rng))

  private def sparseVec(n: Int, width: Int, rng: Random): Seq[BigInt] =
    Seq.tabulate(n)(i => if (rng.nextInt(5) == 0) BigInt(width, rng) else BigInt(0))

  private def runOne(dut: ParamCore, p: CoreParams, a: Seq[BigInt], b: Seq[BigInt]): Unit = {
    val expected = schoolbookNegacyclic(a, b, p.n, p.outW)
    dut.io.valid_in.poke(true.B)
    for (i <- 0 until p.n) {
      dut.io.a(i).poke(a(i).U)
      dut.io.b(i).poke(b(i).U)
    }
    dut.clock.step(1)
    dut.io.valid_in.poke(false.B)

    var timeout = 0
    while (!dut.io.valid_out.peek().litToBoolean && timeout < 300) {
      dut.clock.step(1)
      timeout += 1
    }
    assert(timeout < 300, s"valid_out timeout for $p\na=$a\nb=$b")

    val actual = (0 until p.n).map(i => dut.io.c(i).peek().litValue)
    val mismatch = expected.zip(actual).zipWithIndex.find { case ((e, g), _) => e != g }
    mismatch.foreach { case ((e, g), idx) =>
      fail(
        s"""ParamCore mismatch
           |n=${p.n}
           |aInW=${p.aInW}
           |bInW=${p.bInW}
           |params=$p
           |input a=${a.map(v => "0x" + v.toString(16))}
           |input b=${b.map(v => "0x" + v.toString(16))}
           |expected=${expected.map(v => "0x" + v.toString(16))}
           |actual=${actual.map(v => "0x" + v.toString(16))}
           |mismatch index=$idx expected=0x${e.toString(16)} actual=0x${g.toString(16)}
           |""".stripMargin
      )
    }
  }

  private def directedCases(p: CoreParams, rng: Random): Seq[(Seq[BigInt], Seq[BigInt])] = {
    val maxA = mask(p.aInW)
    val maxB = mask(p.bInW)
    val zerosA = Seq.fill(p.n)(BigInt(0))
    val zerosB = Seq.fill(p.n)(BigInt(0))
    val aOne = Seq.tabulate(p.n)(i => if (i == p.n / 3) BigInt(1) else BigInt(0))
    val bOne = Seq.tabulate(p.n)(i => if (i == p.n / 2) BigInt(1) else BigInt(0))
    Seq(
      zerosA -> zerosB,
      aOne -> randomVec(p.n, p.bInW, rng),
      randomVec(p.n, p.aInW, rng) -> bOne,
      Seq.fill(p.n)(maxA) -> Seq.fill(p.n)(maxB),
      Seq.tabulate(p.n)(i => if (i % 2 == 0) BigInt(0) else maxA) -> Seq.tabulate(p.n)(i => if (i % 2 == 0) maxB else BigInt(0)),
      sparseVec(p.n, p.aInW, rng) -> sparseVec(p.n, p.bInW, rng),
      randomVec(p.n, p.aInW, rng) -> randomVec(p.n, p.bInW, rng)
    )
  }

  behavior of "ParamCore"

  it should "match schoolbook for directed and random n/width configurations" in {
    for {
      n <- Seq(4, 16, 64)
      aW <- Seq(24, 28, 32, 36)
      bW <- Seq(8, 10, 12, 14, 16)
    } {
      val p = CoreWidthPolicy.defaultFor(n, aW, bW, aW)
      test(new ParamCore(p)) { dut =>
        val rng = new Random(0x544334L + n * 1000L + aW * 37L + bW)
        val cases = directedCases(p, rng) ++ Seq.fill(20)(randomVec(n, aW, rng) -> randomVec(n, bW, rng))
        cases.foreach { case (a, b) => runOne(dut, p, a, b) }
      }
    }
  }

  it should "cover the original toomcook16-compatible 24x8 to 24-bit path" in {
    // This is the replacement path for the original toomcook16 kernel shape:
    // n=16, four lanes per evaluation point, seven dot_product sub-cores, and
    // the same interpolation cWire negacyclic folding, checked against schoolbook.
    val p = CoreWidthPolicy.defaultFor(n = 16, aInW = 24, bInW = 8, targetOutW = 24)
    test(new ParamCore(p)) { dut =>
      val rng = new Random(20260508L)
      val cases = directedCases(p, rng) ++ Seq.fill(20)(randomVec(16, 24, rng) -> randomVec(16, 8, rng))
      cases.foreach { case (a, b) => runOne(dut, p, a, b) }
    }
  }
}
