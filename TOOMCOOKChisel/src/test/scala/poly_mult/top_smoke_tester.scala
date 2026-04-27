package poly_mult

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

// 简洁冒烟测试：验证顶层能启动并在有限周期内给出 valid_out
class TopSmokeTest extends AnyFlatSpec with ChiselScalatestTester {
  "top" should "produce valid_out eventually" in {
    test(new top) { dut =>
      for (i <- 0 until 1024) {
        dut.io.a(i).poke((i & 0xFFFFFF).U(24.W))
        dut.io.b(i).poke((i & 0xFF).U(8.W))
      }

      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      var timeout = 0
      while (!dut.io.valid_out.peek().litToBoolean && timeout < 20000) {
        dut.clock.step(1)
        timeout += 1
      }

      assert(timeout < 20000, "timeout: valid_out did not assert")
    }
  }
}
