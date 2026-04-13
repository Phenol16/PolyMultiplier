package poly_mult
import chisel3._
import chisel3.util._

class evalIO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(16, UInt(24.W)))
  val b = Input(Vec(16, UInt(8.W)))
  val valid_out = Output(Bool())
  val A_eval = Output(Vec(28, UInt(30.W))) // 24 + 3 + 3
  val B_eval = Output(Vec(28, UInt(16.W))) // 8 + 4 + 4
}

class evaluation extends Module {
  val io = IO(new evalIO)
  private def fillMsb(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) {
      value(targetWidth - 1, 0)
    } else {
      Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
    }
  }
  for (j <- 0 until 4) {
    // ===== A eval (24bit input, 30bit output) =====
    val ar0 = io.a(j * 4)
    val ar1 = io.a(j * 4 + 1)
    val ar2 = io.a(j * 4 + 2)
    val ar3 = io.a(j * 4 + 3)

    val a_even = ar0 +& ar2
    val a_odd = ar1 +& ar3
    val a_scaled_even = Cat(ar0, 0.U(2.W)) +& ar2
    val a_scaled_odd = Cat(ar1, 0.U(2.W)) +& ar3

    val ah0 = ar2 +& Cat(ar3, 0.U(1.W))
    val ah1 = ar1 +& Cat(ah0, 0.U(1.W))
    val ah2 = ar0 +& Cat(ah1, 0.U(1.W))

    io.A_eval(j * 7 + 0) := ar3
    io.A_eval(j * 7 + 1) := ah2
    io.A_eval(j * 7 + 2) := a_even +& a_odd
    io.A_eval(j * 7 + 3) := fillMsb(a_even -& a_odd, 30)
    io.A_eval(j * 7 + 4) := Cat(a_scaled_even, 0.U(1.W)) +& a_scaled_odd
    io.A_eval(j * 7 + 5) := fillMsb(
      Cat(a_scaled_even, 0.U(1.W)) -& a_scaled_odd,
      30
    )
    io.A_eval(j * 7 + 6) := ar0

    // ===== B eval (8bit input, 16bit output) =====
    val br0 = io.b(j * 4)
    val br1 = io.b(j * 4 + 1)
    val br2 = io.b(j * 4 + 2)
    val br3 = io.b(j * 4 + 3)

    val b_even = br0 +& br2
    val b_odd = br1 +& br3
    val b_scaled_even = Cat(br0, 0.U(2.W)) +& br2
    val b_scaled_odd = Cat(br1, 0.U(2.W)) +& br3

    val bh0 = br2 +& Cat(br3, 0.U(1.W))
    val bh1 = br1 +& Cat(bh0, 0.U(1.W))
    val bh2 = br0 +& Cat(bh1, 0.U(1.W))

    io.B_eval(j * 7 + 0) := br3
    io.B_eval(j * 7 + 1) := bh2
    io.B_eval(j * 7 + 2) := b_even +& b_odd
    io.B_eval(j * 7 + 3) := fillMsb(b_even -& b_odd, 16)
    io.B_eval(j * 7 + 4) := Cat(b_scaled_even, 0.U(1.W)) +& b_scaled_odd
    io.B_eval(j * 7 + 5) := fillMsb(
      Cat(b_scaled_even, 0.U(1.W)) -& b_scaled_odd,
      16
    )
    io.B_eval(j * 7 + 6) := br0
  }
  io.valid_out := io.valid_in
}
