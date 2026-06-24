package poly_mult
import chisel3._
import chisel3.util._

class core4IO(
    aWidth: Int = 30,
    bWidth: Int = 16,
    outWidth: Int = 27
) extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(4, UInt(aWidth.W)))
  val b = Input(Vec(4, UInt(bWidth.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(4, UInt(outWidth.W)))
}

class core4(
    aWidth: Int = 30,
    bWidth: Int = 16,
    mulWidth: Int = 30,
    outWidth: Int = 27,
    inv3: BigInt = BigInt("2AAAAAAB", 16),
    inv9: BigInt = BigInt("38E38E39", 16),
    inv18: BigInt = BigInt("2EEEEEEF", 16)
) extends Module {
  require(aWidth >= bWidth, "core4 expects aWidth >= bWidth for signed B extension")
  require(mulWidth >= outWidth, "core4 expects mulWidth >= outWidth")

  val io = IO(new core4IO(aWidth, bWidth, outWidth))

  private def mask(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, 0.U), value)
  }

  private def fillMsb(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
  }

  private def signedMul(a: UInt, b: UInt): UInt = {
    val aSigned = fillMsb(a, aWidth + 1).asSInt
    val bSigned = fillMsb(b, bWidth + 1).asSInt
    mask((aSigned * bSigned).asUInt, mulWidth)
  }

  val ar0 = io.a(0); val ar1 = io.a(1)
  val ar2 = io.a(2); val ar3 = io.a(3)

  val a_even = ar0 +& ar2
  val a_odd = ar1 +& ar3
  val a_scaled_even = Cat(ar0, 0.U(2.W)) +& ar2
  val a_scaled_odd = Cat(ar1, 0.U(2.W)) +& ar3

  val ah0 = ar2 +& Cat(ar3, 0.U(1.W))
  val ah1 = ar1 +& Cat(ah0, 0.U(1.W))
  val ah2 = ar0 +& Cat(ah1, 0.U(1.W))

  val A_eval = Wire(Vec(7, UInt(aWidth.W)))
  A_eval(0) := mask(ar3, aWidth)
  A_eval(1) := mask(ah2, aWidth)
  A_eval(2) := mask(a_even +& a_odd, aWidth)
  A_eval(3) := fillMsb(a_even -& a_odd, aWidth)
  A_eval(4) := mask(Cat(a_scaled_even, 0.U(1.W)) +& a_scaled_odd, aWidth)
  A_eval(5) := fillMsb(Cat(a_scaled_even, 0.U(1.W)) -& a_scaled_odd, aWidth)
  A_eval(6) := mask(ar0, aWidth)

  val br0 = io.b(0); val br1 = io.b(1)
  val br2 = io.b(2); val br3 = io.b(3)

  val b_even = br0 +& br2
  val b_odd = br1 +& br3
  val b_scaled_even = Cat(br0, 0.U(2.W)) +& br2
  val b_scaled_odd = Cat(br1, 0.U(2.W)) +& br3

  val bh0 = br2 +& Cat(br3, 0.U(1.W))
  val bh1 = br1 +& Cat(bh0, 0.U(1.W))
  val bh2 = br0 +& Cat(bh1, 0.U(1.W))

  val B_eval = Wire(Vec(7, UInt(bWidth.W)))
  B_eval(0) := mask(br3, bWidth)
  B_eval(1) := mask(bh2, bWidth)
  B_eval(2) := mask(b_even +& b_odd, bWidth)
  B_eval(3) := fillMsb(b_even -& b_odd, bWidth)
  B_eval(4) := mask(Cat(b_scaled_even, 0.U(1.W)) +& b_scaled_odd, bWidth)
  B_eval(5) := fillMsb(Cat(b_scaled_even, 0.U(1.W)) -& b_scaled_odd, bWidth)
  B_eval(6) := mask(br0, bWidth)

  // 打一拍：evaluation → multiplication
  val s1_valid = RegNext(io.valid_in, false.B)
  val s1_A = RegNext(A_eval)
  val s1_B = RegNext(B_eval)

  // multiplication
  val w = Wire(Vec(7, UInt(mulWidth.W)))
  for (i <- 0 until 7) {
    w(i) := signedMul(s1_A(i), s1_B(i))
  }

  // 打一拍：multiplication → interpolation
  val s2_valid = RegNext(s1_valid, false.B)
  val s2_w = RegNext(w)

  // Interpolation
  val iw = s2_w

  val r1a = mask(iw(1) + iw(4), mulWidth)
  val r5a = mask(iw(5) - iw(4), mulWidth)
  val r3a = mask(mask(iw(3) - iw(2), mulWidth) >> 1, mulWidth)
  val r4a = mask(iw(4) - iw(0), mulWidth)

  val r4b = mask((r4a << 1) + r5a - (iw(6) << 7), mulWidth)
  val r2a = mask(iw(2) + r3a, mulWidth)

  val r1b = mask(r1a - (r2a << 6) - r2a, mulWidth)
  val r2b = mask(r2a - iw(6) - iw(0), mulWidth)

  val r1c = mask(r1b + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), mulWidth)

  // 三个乘法并行
  val r4d = mask(mask(r4b - (r2b << 3), mulWidth) >> 3, mulWidth) * inv3.U((mulWidth + 6).W)
  val r1e = mask(mask(r1c + (r3a << 4), mulWidth) >> 1, mulWidth) * inv9.U((mulWidth + 6).W)
  val r5c = mask((r5a + r1c) >> 1, mulWidth) * inv18.U((mulWidth + 6).W)

  val r4f = mask(r4d, mulWidth)
  val r1f = mask(r1e, mulWidth)
  val r5f = mask(r5c, mulWidth)

  val r2c = mask(r2b - r4f, mulWidth)
  val r3final = mask(0.U - r3a - r1f, mulWidth)
  val r5d = mask((r1f - r5f) >> 1, mulWidth)
  val r1final = mask(r1f - r5d, mulWidth)

  val outC = Wire(Vec(4, UInt(outWidth.W)))
  outC(0) := mask(iw(6) - r2c, outWidth)
  outC(1) := mask(r5d - r1final, outWidth)
  outC(2) := mask(r4f - iw(0), outWidth)
  outC(3) := mask(r3final, outWidth)

  io.valid_out := s2_valid
  io.c := outC
}
