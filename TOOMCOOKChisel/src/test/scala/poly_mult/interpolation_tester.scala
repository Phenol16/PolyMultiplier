package poly_mult

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class InterpTest extends AnyFlatSpec with ChiselScalatestTester {

  // ---------------------------------------------------------------
  // 黄金参考模型：完全照搬 C 代码逻辑，在 Scala 中模拟
  // w(row)(col)，返回 c(0..15)
  // ---------------------------------------------------------------
  def refModel(w: Array[Array[Long]]): Array[Long] = {
    val MASK27 = 0x7ffffffL
    val MASK24 = 0xffffffL

    // Scala Long 做无符号 27-bit 运算：截断后负数自动变为正的补码表示
    def m(x: Long): Long = x & MASK27

    val c = Array.fill(16)(0L)
    val r = Array.fill(10)(0L)

    for (i <- 0 until 4) {
      r(7) = r(0)
      r(8) = r(1)
      r(9) = r(2)
      r(0) = w(0)(i)
      r(1) = w(1)(i)
      r(2) = w(2)(i)
      r(3) = w(3)(i)
      r(4) = w(4)(i)
      r(5) = w(5)(i)
      r(6) = w(6)(i)

      r(1) = m(r(1) + r(4))
      r(5) = m(r(5) - r(4))
      r(3) = m((r(3) - r(2)) >> 1)

      r(4) = m(((r(4) - r(0)) << 1) + r(5) - (r(6) << 7))
      r(2) = m(r(2) + r(3))
      r(1) = m(r(1) - (r(2) << 6) - r(2))

      r(2) = m(r(2) - r(6) - r(0))
      r(1) = m(r(1) + r(2) + (r(2) << 2) + (r(2) << 3) + (r(2) << 5))

      r(4) = m(m((r(4) - (r(2) << 3)) >> 3) * 0xaaaaaabL)
      r(5) = m(r(5) + r(1))

      r(1) = m(m((r(1) + (r(3) << 4)) >> 1) * 0x8e38e39L)
      r(3) = m(-r(3) - r(1))

      r(5) = m((r(1) - m((r(5) >> 1) * 0xeeeeeefL)) >> 1)
      r(2) = m(r(2) - r(4))

      r(1) = m(r(1) - r(5))

      c(4 * i + 3) = r(3)
      if (i == 0) {
        c(4 * i) = r(6)
        c(4 * i + 1) = r(5)
        c(4 * i + 2) = r(4)
      } else {
        c(4 * i) = r(6) + r(9)
        c(4 * i + 1) = r(5) + r(8)
        c(4 * i + 2) = r(4) + r(7)
      }
    }

    c(0) = c(0) - r(2)
    c(1) = c(1) - r(1)
    c(2) = c(2) - r(0)

    for (i <- 0 until 16) { c(i) = c(i) & MASK24 }
    c
  }

  // ---------------------------------------------------------------
  // 辅助：将二维 w(row)(col) 展平为 io.w(row*4+col) 顺序的一维数组
  // ---------------------------------------------------------------
  def flattenW(w: Array[Array[Long]]): Array[Long] = {
    val flat = Array.fill(28)(0L)
    for (row <- 0 until 7; col <- 0 until 4) {
      flat(row * 4 + col) = w(row)(col)
    }
    flat
  }

  // ---------------------------------------------------------------
  // 驱动 DUT，等待 valid_out，读取 c 输出
  // ---------------------------------------------------------------
  def runDut(dut: interpolation, wFlat: Array[Long]): Array[Long] = {
    // 写输入
    dut.io.valid_in.poke(true.B)
    for (i <- 0 until 28) {
      dut.io.w(i).poke(wFlat(i).U)
    }
    dut.clock.step(1)
    dut.io.valid_in.poke(false.B)

    // 最多等 10 拍，等 valid_out
    var found = false
    for (_ <- 0 until 10) {
      if (!found && dut.io.valid_out.peek().litToBoolean) {
        found = true
      } else if (!found) {
        dut.clock.step(1)
      }
    }
    assert(found, "valid_out never went high within 10 cycles")

    // 读取输出
    Array.tabulate(16)(i => dut.io.c(i).peek().litValue.toLong)
  }

  // ---------------------------------------------------------------
  // 测试 1：全零输入，输出应全零
  // ---------------------------------------------------------------
  "interpolation" should "output all zeros for all-zero input" in {
    test(new interpolation) { dut =>
      val w = Array.fill(7)(Array.fill(4)(0L))
      val gold = refModel(w)
      val out = runDut(dut, flattenW(w))

      for (i <- 0 until 16) {
        assert(
          out(i) == gold(i),
          s"c[$i]: got 0x${out(i).toHexString}, expected 0x${gold(i).toHexString}"
        )
      }
    }
  }

  // ---------------------------------------------------------------
  // 测试 2：全 1（0x7FFFFFF）输入
  // ---------------------------------------------------------------
  "interpolation" should "handle all-ones input" in {
    test(new interpolation) { dut =>
      val w = Array.fill(7)(Array.fill(4)(0x7ffffffL))
      val gold = refModel(w)
      val out = runDut(dut, flattenW(w))

      for (i <- 0 until 16) {
        assert(
          out(i) == gold(i),
          s"c[$i]: got 0x${out(i).toHexString}, expected 0x${gold(i).toHexString}"
        )
      }
    }
  }

  // ---------------------------------------------------------------
  // 测试 3：只有 w[0][0] = 1，其余为 0（单点激励）
  // ---------------------------------------------------------------
  "interpolation" should "handle single non-zero element" in {
    test(new interpolation) { dut =>
      val w = Array.fill(7)(Array.fill(4)(0L))
      w(0)(0) = 1L
      val gold = refModel(w)
      val out = runDut(dut, flattenW(w))

      for (i <- 0 until 16) {
        assert(
          out(i) == gold(i),
          s"c[$i]: got 0x${out(i).toHexString}, expected 0x${gold(i).toHexString}"
        )
      }
    }
  }

  // ---------------------------------------------------------------
  // 测试 4：固定已知输入，验证参考模型与 DUT 一致
  // ---------------------------------------------------------------
  "interpolation" should "match reference on a fixed known input" in {
    test(new interpolation) { dut =>
      // 用一组手工构造的值，各行各列取不同数
      val w = Array(
        Array(0x0000001L, 0x0000002L, 0x0000004L, 0x0000008L),
        Array(0x0000010L, 0x0000020L, 0x0000040L, 0x0000080L),
        Array(0x0000100L, 0x0000200L, 0x0000400L, 0x0000800L),
        Array(0x0001000L, 0x0002000L, 0x0004000L, 0x0008000L),
        Array(0x0010000L, 0x0020000L, 0x0040000L, 0x0080000L),
        Array(0x0100000L, 0x0200000L, 0x0400000L, 0x0600000L),
        Array(0x0123456L, 0x0234567L, 0x0345678L, 0x0456789L)
      )
      val gold = refModel(w)
      val out = runDut(dut, flattenW(w))

      for (i <- 0 until 16) {
        assert(
          out(i) == gold(i),
          s"c[$i]: got 0x${out(i).toHexString}, expected 0x${gold(i).toHexString}"
        )
      }
    }
  }

  // ---------------------------------------------------------------
  // 测试 5：随机输入，跑多组（伪随机，种子固定保证可复现）
  // ---------------------------------------------------------------
  "interpolation" should "match reference on random inputs" in {
    val rng = new scala.util.Random(0xdeadbeefL)
    val MASK = 0x7ffffffL

    for (trial <- 0 until 20) {
      test(new interpolation) { dut =>
        val w = Array.fill(7)(Array.fill(4)((rng.nextLong() & MASK)))
        val gold = refModel(w)
        val out = runDut(dut, flattenW(w))

        for (i <- 0 until 16) {
          assert(
            out(i) == gold(i),
            s"Trial $trial, c[$i]: got 0x${out(i).toHexString}, expected 0x${gold(i).toHexString}"
          )
        }
      }
    }
  }

  // ---------------------------------------------------------------
  // 测试 6：连续两次激励，验证状态机能正确复位重跑
  // ---------------------------------------------------------------
  "interpolation" should "produce correct results on back-to-back valid_in pulses" in {
    test(new interpolation) { dut =>
      val rng = new scala.util.Random(42)
      val MASK = 0x7ffffffL

      for (round <- 0 until 3) {
        val w = Array.fill(7)(Array.fill(4)((rng.nextLong() & MASK)))
        val gold = refModel(w)
        val out = runDut(dut, flattenW(w))

        for (i <- 0 until 16) {
          assert(
            out(i) == gold(i),
            s"Round $round, c[$i]: got 0x${out(i).toHexString}, expected 0x${gold(i).toHexString}"
          )
        }
        // 回到 idle，再拍一次让 sDone→sIdle
        dut.clock.step(1)
      }
    }
  }
}
