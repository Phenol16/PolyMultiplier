package poly_mult
import chisel3._
import chisel3.util._

class dotIO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(4, UInt(30.W)))
  val b = Input(Vec(4, UInt(16.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(4, UInt(27.W)))
}

class dot_product extends Module {
  val io = IO(new dotIO)
  private def fillMsb(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) {
      value(targetWidth - 1, 0)
    } else {
      Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
    }
  }

  // Stage 0: input register
  val s0_valid = RegNext(io.valid_in, false.B)
  val s0_a = RegNext(io.a)
  val s0_b = RegNext(io.b)

  // Stage 1: evaluation (style aligned with evaluation.scala)
  val ar0 = s0_a(0)
  val ar1 = s0_a(1)
  val ar2 = s0_a(2)
  val ar3 = s0_a(3)

  val a_even = ar0 +& ar2
  val a_odd = ar1 +& ar3
  val a_scaled_even = Cat(ar0, 0.U(2.W)) +& ar2
  val a_scaled_odd = Cat(ar1, 0.U(2.W)) +& ar3

  val ah0 = ar2 +& Cat(ar3, 0.U(1.W))
  val ah1 = ar1 +& Cat(ah0, 0.U(1.W))
  val ah2 = ar0 +& Cat(ah1, 0.U(1.W))

  val A_eval = Wire(Vec(7, UInt(30.W)))
  A_eval(0) := ar3
  A_eval(1) := ah2
  A_eval(2) := a_even +& a_odd
  A_eval(3) := fillMsb(a_even -& a_odd, 30)
  A_eval(4) := Cat(a_scaled_even, 0.U(1.W)) +& a_scaled_odd
  A_eval(5) := fillMsb(Cat(a_scaled_even, 0.U(1.W)) -& a_scaled_odd, 30)
  A_eval(6) := ar0

  val br0 = s0_b(0)
  val br1 = s0_b(1)
  val br2 = s0_b(2)
  val br3 = s0_b(3)

  val b_even = br0 +& br2
  val b_odd = br1 +& br3
  val b_scaled_even = Cat(br0, 0.U(2.W)) +& br2
  val b_scaled_odd = Cat(br1, 0.U(2.W)) +& br3

  val bh0 = br2 +& Cat(br3, 0.U(1.W))
  val bh1 = br1 +& Cat(bh0, 0.U(1.W))
  val bh2 = br0 +& Cat(bh1, 0.U(1.W))

  val B_eval = Wire(Vec(7, UInt(16.W)))
  B_eval(0) := br3
  B_eval(1) := bh2
  B_eval(2) := b_even +& b_odd
  B_eval(3) := fillMsb(b_even -& b_odd, 16)
  B_eval(4) := Cat(b_scaled_even, 0.U(1.W)) +& b_scaled_odd
  B_eval(5) := fillMsb(Cat(b_scaled_even, 0.U(1.W)) -& b_scaled_odd, 16)
  B_eval(6) := br0

  val s1_valid = RegNext(s0_valid, false.B)
  val s1_A = RegNext(A_eval)
  val s1_B = RegNext(B_eval)

  // Stage 2: pointwise multiplication (signed modulo 2^27)
  private def mulSignedMq28Q13(a: UInt, b: UInt): UInt = {
    val aSigned = Cat(a(29), a(26, 0)).asSInt // signed m_q28
    val bSigned = Cat(b(15), b(11, 0)).asSInt // signed q13
    (aSigned * bSigned).asUInt()(26, 0)
  }

  val w_comb = VecInit((0 until 7).map(i => mulSignedMq28Q13(s1_A(i), s1_B(i))))

  val s2_valid = RegNext(s1_valid, false.B)
  val s2_w = RegNext(w_comb)

  // Stage 3: interpolation (same equations as toomcook44 modulo 2^27)
  val w = s2_w

  val r1_v1 = (w(1) + w(4))(26, 0)
  val r5_v1 = (w(5) - w(4))(26, 0)
  val r3_v1 = ((w(3) - w(2)) >> 1)
  val r4_v1 = (w(4) - w(0))(26, 0)

  val r4_v2 = ((r4_v1 << 1) + r5_v1 - (w(6) << 7))(26, 0)
  val r2_v1 = (w(2) + r3_v1)(26, 0)
  val r1_v2 = (r1_v1 - (r2_v1 << 6) - r2_v1)(26, 0)
  val r2_v2 = (r2_v1 - w(6) - w(0))(26, 0)

  val s3_valid_mid = RegNext(s2_valid, false.B)
  val s3_r1_mid = RegNext(r1_v2)
  val s3_r2_mid = RegNext(r2_v2)
  val s3_r3_mid = RegNext(r3_v1)
  val s3_r4_mid = RegNext(r4_v2)
  val s3_r5_mid = RegNext(r5_v1)
  val s3_w0_mid = RegNext(w(0))
  val s3_w6_mid = RegNext(w(6))

  val r1_v3 =
    (s3_r1_mid + s3_r2_mid + (s3_r2_mid << 2) + (s3_r2_mid << 3) + (s3_r2_mid << 5))(26, 0)
  val r4_final = (((s3_r4_mid - (s3_r2_mid << 3)) >> 3).asUInt * "hAAAAAAB".U)(26, 0)

  val r5_v2 = (s3_r5_mid + r1_v3)(26, 0)
  val r1_v4 = (((r1_v3 + (s3_r3_mid << 4)) >> 1).asUInt * "h8E38E39".U)(26, 0)
  val r2_v3 = (s3_r2_mid - r4_final)(26, 0)

  val r3_final = (0.U - s3_r3_mid - r1_v4)(26, 0)
  val r5_final = ((r1_v4 - ((r5_v2 >> 1).asUInt * "hEEEEEEF".U)(26, 0)) >> 1)
  val r1_final = (r1_v4 - r5_final)(26, 0)

  io.valid_out := RegNext(s3_valid_mid, false.B)

  val out_c = Wire(Vec(4, UInt(27.W)))
  out_c(0) := (s3_w6_mid - r2_v3)(26, 0)
  out_c(1) := (r5_final - r1_final)(26, 0)
  out_c(2) := (r4_final - s3_w0_mid)(26, 0)
  out_c(3) := r3_final

  io.c := RegNext(out_c)
}
