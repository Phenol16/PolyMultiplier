package poly_mult
import chisel3._
import chisel3.util._

class evalIO(
    aWidth: Int = 24,
    bWidth: Int = 8,
    aEvalWidth: Int = 30,
    bEvalWidth: Int = 16
) extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(16, UInt(aWidth.W)))
  val b = Input(Vec(16, UInt(bWidth.W)))
  val valid_out = Output(Bool())
  val A_eval = Output(Vec(28, UInt(aEvalWidth.W)))
  val B_eval = Output(Vec(28, UInt(bEvalWidth.W)))
}

class evaluation(
    aWidth: Int = 24,
    bWidth: Int = 8,
    aEvalWidth: Int = 30,
    bEvalWidth: Int = 16
) extends Module {
  val io = IO(new evalIO(aWidth, bWidth, aEvalWidth, bEvalWidth))

  private def mask(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, 0.U), value)
  }

  private def fillMsb(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) {
      value(targetWidth - 1, 0)
    } else {
      Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
    }
  }

  for (j <- 0 until 4) {
    // ===== A eval =====
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

    io.A_eval(j + 0) := mask(ar3, aEvalWidth)
    io.A_eval(j + 4) := mask(ah2, aEvalWidth)
    io.A_eval(j + 8) := mask(a_even +& a_odd, aEvalWidth)
    io.A_eval(j + 12) := fillMsb(a_even -& a_odd, aEvalWidth)
    io.A_eval(j + 16) := mask(Cat(a_scaled_even, 0.U(1.W)) +& a_scaled_odd, aEvalWidth)
    io.A_eval(j + 20) := fillMsb(
      Cat(a_scaled_even, 0.U(1.W)) -& a_scaled_odd,
      aEvalWidth
    )
    io.A_eval(j + 24) := mask(ar0, aEvalWidth)

    // ===== B eval =====
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

    io.B_eval(j + 0) := mask(br3, bEvalWidth)
    io.B_eval(j + 4) := mask(bh2, bEvalWidth)
    io.B_eval(j + 8) := mask(b_even +& b_odd, bEvalWidth)
    io.B_eval(j + 12) := fillMsb(b_even -& b_odd, bEvalWidth)
    io.B_eval(j + 16) := mask(Cat(b_scaled_even, 0.U(1.W)) +& b_scaled_odd, bEvalWidth)
    io.B_eval(j + 20) := fillMsb(
      Cat(b_scaled_even, 0.U(1.W)) -& b_scaled_odd,
      bEvalWidth
    )
    io.B_eval(j + 24) := mask(br0, bEvalWidth)
  }
  io.valid_out := io.valid_in
}
