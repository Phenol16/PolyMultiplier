package poly_mult
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ToomCook44Test extends AnyFlatSpec with ChiselScalatestTester {
  "ToomCook44" should "show internal values" in {
    test(new ToomCook44) { dut =>
      // 1. 设置输入值
      dut.io.valid_in.poke(true.B)
      for (i <- 0 until 4) {
        dut.io.a(i).poke((2 * i + 1).U)
        dut.io.b(i).poke((10 * i).U)
      }

      // 2. 步进时钟
      // Stage 0 -> Stage 1 需要 1 个周期
      dut.clock.step(1)

      // 3. 步进到 Stage 2 (此时 A_eval 和 B_eval 已经计算出并存入 s1_A, s1_B)
      dut.clock.step(1)

      // 4. 步进到 Stage 3 (此时 w_comb 已经计算出并存入 s2_w)
      dut.clock.step(1)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)
      dut.clock.step(2)
    }
  }
}
