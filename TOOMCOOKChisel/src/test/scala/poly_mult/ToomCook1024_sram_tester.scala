package poly_mult_sram

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random
import java.time.LocalTime

class ToomCook1024Test extends AnyFlatSpec with ChiselScalatestTester {

  private def now(): String = LocalTime.now().toString

  private val N = 1024
  private val QMask: BigInt = (BigInt(1) << 24) - 1

  /**
    * negacyclic convolution modulo x^1024 + 1, then modulo 2^24.
    *
    * c[j] += a[i] * b[j - i]       if j >= i
    * c[j] -= a[i] * b[n + j - i]   if j < i
    */
  private def schoolbookNegacyclic(
      a: Seq[BigInt],
      b: Seq[BigInt],
      n: Int = N
  ): Seq[BigInt] = {
    val c = Array.fill(n)(BigInt(0))

    for (i <- 0 until n) {
      for (j <- i until n) {
        c(j) = c(j) + a(i) * b(j - i)
      }
      for (j <- 0 until i) {
        c(j) = c(j) - a(i) * b(n + j - i)
      }
    }

    c.map(x => x & QMask).toSeq
  }

  private def zeroVec: Seq[BigInt] = Seq.fill(N)(BigInt(0))

  private def oneHot(pos: Int, value: BigInt = BigInt(1)): Seq[BigInt] = {
    require(pos >= 0 && pos < N, s"oneHot position out of range: $pos")
    val arr = Array.fill(N)(BigInt(0))
    arr(pos) = value
    arr.toSeq
  }

  private def runCase(
      dut: ToomCook43,
      label: String,
      aVals: Seq[BigInt],
      bVals: Seq[BigInt],
      expected: Seq[BigInt],
      maxWaitCycles: Int = 6000,
      maxPrintMismatch: Int = 30,
      printNonZero: Boolean = true
  ): Unit = {
    require(aVals.length == N, s"$label: a length must be $N")
    require(bVals.length == N, s"$label: b length must be $N")
    require(expected.length == N, s"$label: expected length must be $N")

    println(s"[${now()}][$label] start poking inputs")

    for (i <- 0 until N) {
      dut.io.a(i).poke((aVals(i) & QMask).U)
      dut.io.b(i).poke((bVals(i) & BigInt("ff", 16)).U)
    }

    println(s"[${now()}][$label] finish poking inputs")
    println(s"[${now()}][$label] send valid_in")

    dut.io.valid_in.poke(true.B)
    dut.clock.step(1)
    dut.io.valid_in.poke(false.B)

    println(s"[${now()}][$label] start waiting valid_out")

    var seenValid = false
    var outCycle = -1
    var cycle = 0

    while (!seenValid && cycle < maxWaitCycles) {
      val v = dut.io.valid_out.peek().litToBoolean

      if (cycle % 250 == 0) {
        println(s"[${now()}][$label] cycle=$cycle valid_out=$v")
      }

      if (v) {
        seenValid = true
        outCycle = cycle
        println(s"[${now()}][$label] valid_out asserted at cycle=$cycle")
      } else {
        dut.clock.step(1)
        cycle += 1
      }
    }

    assert(
      seenValid,
      s"[$label] Error: 等待 io.valid_out 超时！maxWaitCycles=$maxWaitCycles"
    )

    println(
      s"[$label DEBUG] coreNZ=${dut.io.dbg_core_nonzero.peek().litToBoolean}, " +
        s"w2WriteNZ=${dut.io.dbg_w2_write_nonzero.peek().litToBoolean}, " +
        s"w2ReadNZ=${dut.io.dbg_w2_read_nonzero.peek().litToBoolean}, " +
        s"interp16NZ=${dut.io.dbg_interp16_nonzero.peek().litToBoolean}, " +
        s"w1WriteNZ=${dut.io.dbg_w1_write_nonzero.peek().litToBoolean}, " +
        s"w1ReadNZ=${dut.io.dbg_w1_read_nonzero.peek().litToBoolean}, " +
        s"interp64NZ=${dut.io.dbg_interp64_nonzero.peek().litToBoolean}, " +
        s"w0WriteNZ=${dut.io.dbg_w0_write_nonzero.peek().litToBoolean}, " +
        s"w0RegNZ=${dut.io.dbg_w0_reg_nonzero.peek().litToBoolean}, " +
        s"w0BlockNZ=0b${dut.io.dbg_w0_block_nonzero.peek().litValue.toString(2)}, " +
        s"interp256NZ=${dut.io.dbg_interp256_nonzero.peek().litToBoolean}, " +
        s"finalNZ=${dut.io.dbg_final_nonzero.peek().litToBoolean}"
    )
    println(
      s"[$label DEBUG] coreWriteCount=${dut.io.dbg_core_write_count.peek().litValue}, " +
        s"interp1GroupCount=${dut.io.dbg_interp1_group_count.peek().litValue}, " +
        s"interp2BlockCount=${dut.io.dbg_interp2_block_count.peek().litValue}, " +
        s"w2Ready=0b${dut.io.dbg_w2_ready.peek().litValue.toString(2)}, " +
        s"w2Empty=0b${dut.io.dbg_w2_empty.peek().litValue.toString(2)}, " +
        s"w2Reading=0b${dut.io.dbg_w2_reading.peek().litValue.toString(2)}, " +
        s"w2Writing=0b${dut.io.dbg_w2_writing.peek().litValue.toString(2)}, " +
        s"w1BlockReady=0b${dut.io.dbg_w1_block_ready.peek().litValue.toString(2)}, " +
        s"w0Ready=0b${dut.io.dbg_w0_ready.peek().litValue.toString(2)}"
    )

    println(s"[${now()}][$label] start checking outputs, outCycle=$outCycle")

    var mismatchCount = 0
    val nonZero = scala.collection.mutable.ArrayBuffer[(Int, BigInt)]()

    for (i <- 0 until N) {
      val got = dut.io.c(i).peek().litValue & QMask
      val exp = expected(i) & QMask

      if (got != 0) {
        nonZero += ((i, got))
      }

      if (got != exp) {
        mismatchCount += 1
        if (mismatchCount <= maxPrintMismatch) {
          println(
            s"[$label MISMATCH] index=$i got=0x${got.toString(16)} expected=0x${exp.toString(16)}"
          )
        }
      }
    }

    if (printNonZero) {
      println(s"[${now()}][$label] nonzero output count=${nonZero.length}")
      nonZero.take(30).foreach { case (idx, value) =>
        println(s"[$label NONZERO] c[$idx] = 0x${value.toString(16)}")
      }
    }

    assert(
      mismatchCount == 0,
      s"[$label] 计算错误：共有 $mismatchCount 个系数不匹配，最多已打印 $maxPrintMismatch 个。"
    )

    println(s"[${now()}][$label] PASS, valid_out cycle=$outCycle")

    // 让 DUT 从 DONE 回到 IDLE，便于同一次 elaboration 内连续输入下一组 case
    dut.clock.step(3)
  }

  behavior of "ToomCook1024"

  it should "debug deterministic cases before random test" in {
    println(s"[${now()}] before test(new ToomCook1024)")

    test(new ToomCook43)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>

        dut.clock.setTimeout(0)

        println(s"[${now()}] after elaboration, simulation started")

        // ---------------------------------------------------------------------
        // Case 1: 全 0
        // ---------------------------------------------------------------------
        {
          val a = zeroVec
          val b = zeroVec
          val exp = zeroVec
          runCase(
            dut = dut,
            label = "zero",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 2: a(0)=1, b(0)=1
        // 理论：c(0)=1
        // ---------------------------------------------------------------------
        {
          val a = oneHot(0)
          val b = oneHot(0)
          val exp = schoolbookNegacyclic(a, b)
          runCase(
            dut = dut,
            label = "onehot_a0_b0",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 3: a(1)=1, b(0)=1
        // 理论：c(1)=1
        // ---------------------------------------------------------------------
        {
          val a = oneHot(1)
          val b = oneHot(0)
          val exp = schoolbookNegacyclic(a, b)
          runCase(
            dut = dut,
            label = "onehot_a1_b0",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 4: a(0)=1, b(1)=1
        // 理论：c(1)=1
        // ---------------------------------------------------------------------
        {
          val a = oneHot(0)
          val b = oneHot(1)
          val exp = schoolbookNegacyclic(a, b)
          runCase(
            dut = dut,
            label = "onehot_a0_b1",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 5: a(1023)=1, b(1)=1
        // negacyclic: x^1023 * x = x^1024 = -1
        // 理论：c(0)=2^24-1
        // ---------------------------------------------------------------------
        {
          val a = oneHot(1023)
          val b = oneHot(1)
          val exp = schoolbookNegacyclic(a, b)
          runCase(
            dut = dut,
            label = "onehot_a1023_b1_negacyclic",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = true
          )
        }

        // ---------------------------------------------------------------------
        // Case 6: 小值随机
        // 如果 one-hot 通过但小值随机失败，多半是算法布局/插值问题。
        // ---------------------------------------------------------------------
        {
          val rng = new Random(7)
          val a = Seq.fill(N)(BigInt(rng.nextInt(16)))
          val b = Seq.fill(N)(BigInt(rng.nextInt(16)))
          val exp = schoolbookNegacyclic(a, b)

          runCase(
            dut = dut,
            label = "small_random_4bit",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = false
          )
        }

        // ---------------------------------------------------------------------
        // Case 7: 完整随机
        // 只有前面 case 都通过后，这个才有诊断意义。
        // ---------------------------------------------------------------------
        {
          val rng = new Random(42)
          val a = Seq.fill(N)(BigInt(rng.nextInt() & 0xffffff))
          val b = Seq.fill(N)(BigInt(rng.nextInt() & 0xff))
          val exp = schoolbookNegacyclic(a, b)

          runCase(
            dut = dut,
            label = "full_random_a24_b8",
            aVals = a,
            bVals = b,
            expected = exp,
            printNonZero = false
          )
        }

        println(s"[${now()}] all debug cases passed")
      }
  }
}