package poly_mult
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class ToomCook1024Test extends AnyFlatSpec with ChiselScalatestTester {
  def schoolbook(a: Seq[Long], b: Seq[Long], n: Int): Seq[Long] = {
    val c = Array.fill(n)(0L)

    for (i <- 0 until n) {
      for (j <- i until n) {
        c(j) += a(i) * b(j - i)
      }
      for (j <- 0 until i) {
        c(j) -= a(i) * b(n + j - i)
      }
    }

    // mod q = 2^24
    c.map(val_c => val_c & 0xffffffL).toSeq
  }
  "ToomCook1024" should "work" in {
    test(new ToomCook43) { dut =>
      val n = 1024
      val rng = new Random(42) // 设定固定随机种子，方便出错时复现
      val a_vals = Seq.fill(n)((rng.nextInt() & 0xffffff).toLong)
      val b_vals = Seq.fill(n)((rng.nextInt() & 0xff).toLong)

      val c_expected = schoolbook(a_vals, b_vals, n)

      for (i <- 0 until n) {
        dut.io.a(i).poke(a_vals(i).U)
        dut.io.b(i).poke(b_vals(i).U)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      var timeout = 0
      while (!dut.io.valid_out.peek().litToBoolean && timeout < 1000) {
        dut.clock.step(1)
        timeout += 1
      }
      assert(timeout < 1000, "Error: 等待 io.valid_out 超时！")

      for (i <- 0 until n) {
        dut.io.c(i).expect(c_expected(i).U, s"计算错误！索引 $i 处结果不匹配。")
      }
    }
  }
}