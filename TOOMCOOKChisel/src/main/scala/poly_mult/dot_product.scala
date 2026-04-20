/* package poly_mult
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

  // Stage 2: pointwise multiplication
  private def mulSignedMq30Q17(a: UInt, b: UInt): UInt = {
    val aSigned = Cat(a(29), a(29, 0)).asSInt
    val bSigned = Cat(b(15), b(15, 0)).asSInt
    val product = (aSigned * bSigned).asUInt
    // val product = a * b
    product(29, 0)
  }

  val w_comb = VecInit((0 until 7).map(i => mulSignedMq30Q17(s1_A(i), s1_B(i))))

  val s2_valid = RegNext(s1_valid, false.B)
  val s2_w = RegNext(w_comb)
  /*   when(s2_valid) {
    printf("--- Stage 2 Pointwise multiplication ---\n")
    for (i <- 0 until 7) {
      printf(
        "Point %d: s1_A = 0x%x, s1_B = 0x%x, w_comb = 0x%x\n",
        i.U,
        s1_A(i),
        s1_B(i),
        s2_w(i)
      )
    }
  } */
  // Stage 3: interpolation (same equations as toomcook44 modulo 2^30)
  val w = s2_w

  val r1_v1 = (w(1) + w(4))(29, 0)
  val r5_v1 = (w(5) - w(4))(29, 0)
  val r3_v1 = ((w(3) - w(2)) >> 1)
  val r4_v1 = (w(4) - w(0))(29, 0)

  val r4_v2 = ((r4_v1 << 1) + r5_v1 - (w(6) << 7))(29, 0)
  val r2_v1 = (w(2) + r3_v1)(29, 0)
  val r1_v2 = (r1_v1 - (r2_v1 << 6) - r2_v1)(29, 0)
  val r2_v2 = (r2_v1 - w(6) - w(0))(29, 0)

  val s3_valid_mid = RegNext(s2_valid, false.B)
  val s3_r1_mid = RegNext(r1_v2)
  val s3_r2_mid = RegNext(r2_v2)
  val s3_r3_mid = RegNext(r3_v1)
  val s3_r4_mid = RegNext(r4_v2)
  val s3_r5_mid = RegNext(r5_v1)
  val s3_w0_mid = RegNext(w(0))
  val s3_w6_mid = RegNext(w(6))

  val r1_v3 =
    (s3_r1_mid + s3_r2_mid + (s3_r2_mid << 2) + (s3_r2_mid << 3) + (s3_r2_mid << 5))(
      29,
      0
    )
  val r4_final =
    (((s3_r4_mid - (s3_r2_mid << 3)) >> 3).asUInt * "h2AAAAAAB".U)(29, 0)

  val r5_v2 = (s3_r5_mid + r1_v3)(29, 0)
  val r1_v4 = (((r1_v3 + (s3_r3_mid << 4)) >> 1).asUInt * "h38E38E39".U)(29, 0)
  val r2_v3 = (s3_r2_mid - r4_final)(29, 0)

  val r3_final = (0.U - s3_r3_mid - r1_v4)(29, 0)
  val r5_final = ((r1_v4 - ((r5_v2 >> 1).asUInt * "h2EEEEEEF".U)(29, 0)) >> 1)
  val r1_final = (r1_v4 - r5_final)(29, 0)

  io.valid_out := RegNext(s3_valid_mid, false.B)

  val out_c = Wire(Vec(4, UInt(27.W)))
  out_c(0) := (s3_w6_mid - r2_v3)(26, 0)
  out_c(1) := (r5_final - r1_final)(26, 0)
  out_c(2) := (r4_final - s3_w0_mid)(26, 0)
  out_c(3) := r3_final
  io.c := RegNext(out_c)
}
 */
package poly_mult
import chisel3._
import chisel3.util._

class SignedMulUnit extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(30.W))
    val b = Input(UInt(16.W))
    val c = Output(UInt(30.W))
  })
  val aSigned = Cat(io.a(29), io.a).asSInt
  val bSigned = Cat(io.b(15), io.b).asSInt
  io.c := (aSigned * bSigned).asUInt(29, 0)
}

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
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else
      Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
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

  val A_eval = Wire(Vec(7, UInt(30.W)))
  A_eval(0) := ar3
  A_eval(1) := ah2
  A_eval(2) := a_even +& a_odd
  A_eval(3) := fillMsb(a_even -& a_odd, 30)
  A_eval(4) := Cat(a_scaled_even, 0.U(1.W)) +& a_scaled_odd
  A_eval(5) := fillMsb(Cat(a_scaled_even, 0.U(1.W)) -& a_scaled_odd, 30)
  A_eval(6) := ar0

  val br0 = io.b(0); val br1 = io.b(1)
  val br2 = io.b(2); val br3 = io.b(3)

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

  // 打一拍：evaluation → multiplication

  val s1_valid = RegNext(io.valid_in, false.B)
  val s1_A = RegNext(A_eval)
  val s1_B = RegNext(B_eval)

  // multiplication

  val mulUnits = Seq.fill(7)(Module(new SignedMulUnit))
  for (i <- 0 until 7) {
    mulUnits(i).io.a := s1_A(i)
    mulUnits(i).io.b := s1_B(i)
  }
  val w = VecInit(mulUnits.map(_.io.c))

  // 打一拍：multiplication → interpolation

  val s2_valid = RegNext(s1_valid, false.B)
  val s2_w = RegNext(w)

  // Interpolation

  val MASK30 = "h3FFFFFFF".U
  val MASK27 = "h7FFFFFF".U

  val iw = s2_w

  val r1a = (iw(1) + iw(4)) & MASK30
  val r5a = (iw(5) - iw(4)) & MASK30
  val r3a = ((iw(3) - iw(2)) >> 1) & MASK30
  val r4a = (iw(4) - iw(0)) & MASK30

  val r4b = ((r4a << 1) + r5a - (iw(6) << 7)) & MASK30
  val r2a = (iw(2) + r3a) & MASK30

  val r1b = (r1a - (r2a << 6) - r2a) & MASK30
  val r2b = (r2a - iw(6) - iw(0)) & MASK30

  val r1c = (r1b + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5)) & MASK30

  // 三个乘法并行

  val r4d = (((r4b - (r2b << 3)) & MASK30) >> 3) *
    "h2AAAAAAB".U & MASK30

  val r1e = (((r1c + (r3a << 4)) & MASK30) >> 1) *
    "h38E38E39".U & MASK30

  val r5c = ((r5a + r1c) >> 1) *
    "h2EEEEEEF".U & MASK30

  val r2c = (r2b - r4d) & MASK30
  val r3final = (0.U - r3a - r1e) & MASK30
  val r5d = (r1e - r5c) >> 1
  val r1final = r1e - r5d

  val outC = Wire(Vec(4, UInt(27.W)))
  outC(0) := (iw(6) - r2c)(26, 0)
  outC(1) := (r5d - r1final)(26, 0)
  outC(2) := (r4d - iw(0))(26, 0)
  outC(3) := r3final(26, 0)

  io.valid_out := s2_valid
  io.c := outC
}
