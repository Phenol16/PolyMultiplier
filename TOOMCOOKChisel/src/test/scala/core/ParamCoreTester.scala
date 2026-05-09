package core

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import java.time.LocalTime
import scala.util.Random

class ParamCoreTester extends AnyFlatSpec with ChiselScalatestTester {
  private def now(): String = LocalTime.now().toString

  private def mask(width: Int): BigInt = (BigInt(1) << width) - 1

  private def zeroVec(n: Int): Seq[BigInt] = Seq.fill(n)(BigInt(0))

  private def oneHot(n: Int, pos: Int, value: BigInt = BigInt(1)): Seq[BigInt] = {
    val arr = Array.fill(n)(BigInt(0))
    arr(pos) = value
    arr.toSeq
  }

  private def alternating(width: Int, startWithOne: Boolean): BigInt = {
    (0 until width).foldLeft(BigInt(0)) { case (acc, bit) =>
      val set = if (startWithOne) bit % 2 == 0 else bit % 2 == 1
      if (set) acc | (BigInt(1) << bit) else acc
    }
  }

  private def randomVec(n: Int, width: Int, rng: Random): Seq[BigInt] =
    Seq.fill(n)(BigInt(width, rng))

  private def runOneConfig(coreN: Int, aW: Int, bW: Int, numRandom: Int = 20): Unit = {
    val cfg = CoreConfig.derive(coreN = coreN, aInW = aW, bInW = bW)
    val aMask = mask(aW)
    val bMask = mask(bW)

    test(new ParamCore(cfg))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.clock.setTimeout(0)

        def runCase(label: String, aVals: Seq[BigInt], bVals: Seq[BigInt]): Unit = {
          require(aVals.length == coreN, s"$label: a length must be $coreN")
          require(bVals.length == coreN, s"$label: b length must be $coreN")

          val expected = CoreSchoolbookRef.schoolbookNegacyclic(
            a = aVals,
            b = bVals,
            n = coreN,
            aW = aW,
            bW = bW,
            outW = cfg.coreOutW
          )

          for (i <- 0 until coreN) {
            dut.io.a(i).poke((aVals(i) & aMask).U)
            dut.io.b(i).poke((bVals(i) & bMask).U)
          }

          dut.io.valid_in.poke(true.B)
          dut.clock.step(1)
          dut.io.valid_in.poke(false.B)

          var cycle = 0
          var seenValid = false
          val maxWaitCycles = 8

          while (!seenValid && cycle < maxWaitCycles) {
            if (dut.io.valid_out.peek().litToBoolean) {
              seenValid = true
            } else {
              dut.clock.step(1)
              cycle += 1
            }
          }

          assert(seenValid, s"Timeout waiting for valid_out, cfg=$cfg, label=$label")

          for (i <- 0 until coreN) {
            val got = dut.io.c(i).peek().litValue & mask(cfg.coreOutW)
            val exp = expected(i) & mask(cfg.coreOutW)
            assert(
              got == exp,
              s"Mismatch label=$label coreN=$coreN aW=$aW bW=$bW outW=${cfg.coreOutW} index=$i " +
                s"got=0x${got.toString(16)} expected=0x${exp.toString(16)}"
            )
          }

          dut.clock.step(1)
        }

        println(s"[${now()}] start ParamCore config coreN=$coreN aW=$aW bW=$bW outW=${cfg.coreOutW}")

        runCase("zero", zeroVec(coreN), zeroVec(coreN))
        runCase("onehot_a0_b0", oneHot(coreN, 0), oneHot(coreN, 0))
        runCase("shift_a1_b0", oneHot(coreN, 1), oneHot(coreN, 0))
        runCase("shift_a0_b1", oneHot(coreN, 0), oneHot(coreN, 1))
        runCase("negacyclic_wrap", oneHot(coreN, coreN - 1), oneHot(coreN, 1))

        val rng = new Random(0x5eedL + coreN * 1000L + aW * 100L + bW)
        for (idx <- 0 until numRandom) {
          runCase(
            s"small_random_$idx",
            Seq.fill(coreN)(BigInt(rng.nextInt(16))),
            Seq.fill(coreN)(BigInt(rng.nextInt(16)))
          )
        }
        for (idx <- 0 until numRandom) {
          runCase(s"full_random_$idx", randomVec(coreN, aW, rng), randomVec(coreN, bW, rng))
        }

        val msbA = Array.fill(coreN)(BigInt(0))
        val msbB = Array.fill(coreN)(BigInt(0))
        msbA(0) = BigInt(1) << (aW - 1)
        msbA(coreN / 2) = (BigInt(1) << (aW - 1)) | 1
        msbB(0) = BigInt(1) << (bW - 1)
        msbB(coreN / 2) = (BigInt(1) << (bW - 1)) | 1
        runCase("msb_sign_stress", msbA.toSeq, msbB.toSeq)

        runCase("all_max", Seq.fill(coreN)(aMask), Seq.fill(coreN)(bMask))
        runCase(
          "alternating_bits",
          Seq.fill(coreN)(alternating(aW, startWithOne = false)),
          Seq.fill(coreN)(alternating(bW, startWithOne = true))
        )

        println(s"[${now()}] PASS ParamCore config coreN=$coreN aW=$aW bW=$bW")
      }
  }

  behavior of "ParamCore"

  it should "pass selected parameterized configurations" in {
    runOneConfig(coreN = 4, aW = 24, bW = 8)
    runOneConfig(coreN = 16, aW = 24, bW = 8)
    runOneConfig(coreN = 64, aW = 24, bW = 8, numRandom = 6)
    runOneConfig(coreN = 16, aW = 36, bW = 16)
    runOneConfig(coreN = 64, aW = 36, bW = 16, numRandom = 6)
  }

  ignore should "pass the full 60-configuration sweep" in {
    for {
      coreN <- CoreConfig.supportedCoreNs
      aW <- CoreConfig.supportedAInWs
      bW <- CoreConfig.supportedBInWs
    } runOneConfig(coreN = coreN, aW = aW, bW = bW, numRandom = 4)
  }
}
