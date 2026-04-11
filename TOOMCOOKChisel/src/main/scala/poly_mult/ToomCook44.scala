package poly_mult
import chisel3._
import chisel3.util._

class ToomCook44 extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(4, UInt(24.W)))
    val b = Input(Vec(4, UInt(8.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(4, UInt(24.W)))
  })

  // Stage 0 : Input register

  val s0_valid = RegNext(io.valid_in, false.B)
  val s0_a = RegNext(io.a)
  val s0_b = RegNext(io.b)

  // Stage 1 : Evaluation
  val a28 = VecInit(s0_a.map(_.pad(28)))
  val b13 = VecInit(s0_b.map(_.pad(13)))
  val A_eval = Wire(Vec(7, UInt(24.W)))
  A_eval(0) := a28(3)
  A_eval(1) := a28(0) + (a28(1) << 1) + (a28(2) << 2) + (a28(3) << 3)
  A_eval(2) := a28(0) + a28(1) + a28(2) + a28(3)
  A_eval(3) := a28(0) - a28(1) + a28(2) - a28(3)
  A_eval(4) := (a28(0) << 3) + (a28(1) << 2) + (a28(2) << 1) + a28(3)
  A_eval(5) := (a28(0) << 3) - (a28(1) << 2) + (a28(2) << 1) - a28(3)
  A_eval(6) := a28(0)

  val B_eval = Wire(Vec(7, UInt(12.W)))
  B_eval(0) := b13(3)
  B_eval(1) := b13(0) + (b13(1) << 1) + (b13(2) << 2) + (b13(3) << 3)
  B_eval(2) := b13(0) + b13(1) + b13(2) + b13(3)
  B_eval(3) := b13(0) - b13(1) + b13(2) - b13(3)
  B_eval(4) := (b13(0) << 3) + (b13(1) << 2) + (b13(2) << 1) + b13(3)
  B_eval(5) := (b13(0) << 3) - (b13(1) << 2) + (b13(2) << 1) - b13(3)
  B_eval(6) := b13(0)
  /*   val A_eval=Wire (Vec(7, UInt(24.W)))
  A_eval(0) := s0_a(3)
  A_eval(1) := s0_a(0)+(s0_a(1)<<1)+(s0_a(2)<<2)+(s0_a(3)<<3)
  A_eval(2) := s0_a(0)+s0_a(1)+s0_a(2)+s0_a(3)
  A_eval(3) := s0_a(0)-s0_a(1)+s0_a(2)-s0_a(3)
  A_eval(4) := (s0_a(0)<<3)+(s0_a(1)<<2)+(s0_a(2)<<1)+s0_a(3)
  A_eval(5) := (s0_a(0)<<3)-(s0_a(1)<<2)+(s0_a(2)<<1)-s0_a(3)
  A_eval(6) := s0_a(0)

  val B_eval=Wire (Vec(7, UInt(12.W)))
  B_eval(0) := s0_b(3)
  B_eval(1) := s0_b(0)+(s0_b(1)<<1)+(s0_b(2)<<2)+(s0_b(3)<<3)
  B_eval(2) := s0_b(0)+s0_b(1)+s0_b(2)+s0_b(3)
  B_eval(3) := s0_b(0)-s0_b(1)+s0_b(2)-s0_b(3)
  B_eval(4) := (s0_b(0)<<3)+(s0_b(1)<<2)+(s0_b(2)<<1)+s0_b(3)
  B_eval(5) := (s0_b(0)<<3)-(s0_b(1)<<2)+(s0_b(2)<<1)-s0_b(3)
  B_eval(6) := s0_b(0) */

  val s1_valid = RegNext(s0_valid, false.B)
  val s1_A = RegNext(A_eval)
  val s1_B = RegNext(B_eval)

  /* when(s1_valid) {
  printf("--- Stage 1 Evaluation ---\n")
  for (i <- 0 until 7) {
    printf("Point %d: s1_A = 0x%x, s1_B = 0x%x\n", i.U, s1_A(i), s1_B(i))
  }
} */

// Stage 2 : Pointwise multiplication
  def mulSignedMq28Q13(a: UInt, b: UInt): UInt = {
    val a_signed = Cat(
      a(23),
      a(23),
      a(23),
      a(23),
      a(23, 0)
    ).asSInt // sign-extend to 28-bit SInt
    val b_signed = Cat(b(11), b(11, 0)).asSInt // sign-extend to 13-bit SInt
    val product = (a_signed * b_signed).asUInt // 41-bit result
    product(26, 0)
  }
  val w_comb = VecInit((0 until 7).map { i =>
    mulSignedMq28Q13(s1_A(i), s1_B(i))
  })

  val s2_valid = RegNext(s1_valid, false.B)
  val s2_w = RegNext(w_comb)

  /* when(s2_valid) {
  printf("--- Stage 2 Pointwise multiplication ---\n")
  for (i <- 0 until 7) {
    printf("Point %d: w_comb = 0x%x\n", i.U, s2_w(i))
  }
} */

  // Stage 3 : Interpolation
  val w = s2_w

// --- Part 1 ---
  val r1_v1 = (w(1) + w(4))(26, 0)
  val r5_v1 = (w(5) - w(4))(26, 0)
  val r3_v1 = ((w(3) - w(2)) >> 1)
  val r4_v1 = (w(4) - w(0))(26, 0)

  val r4_v2 = ((r4_v1 << 1) + r5_v1 - (w(6) << 7))(26, 0)
  val r2_v1 = (w(2) + r3_v1)(26, 0)
  val r1_v2 = (r1_v1 - (r2_v1 << 6) - r2_v1)(26, 0)
  val r2_v2 = (r2_v1 - w(6) - w(0))(26, 0)

  // 插入一级寄存器
  val s3_valid_mid = RegNext(s2_valid, false.B)
  val s3_r1_mid = RegNext(r1_v2)
  val s3_r2_mid = RegNext(r2_v2)
  val s3_r3_mid = RegNext(r3_v1)
  val s3_r4_mid = RegNext(r4_v2)
  val s3_r5_mid = RegNext(r5_v1)
  val s3_w0_mid = RegNext(w(0))
  val s3_w6_mid = RegNext(w(6))

// --- Part 2 ---
  val r1_v3 =
    (s3_r1_mid + s3_r2_mid + (s3_r2_mid << 2) + (s3_r2_mid << 3) + (s3_r2_mid << 5))(
      26,
      0
    )
  val r4_final =
    (((s3_r4_mid - (s3_r2_mid << 3)) >> 3).asUInt * "hAAAAAAB".U)(26, 0)

  val r5_v2 = (s3_r5_mid + r1_v3)(26, 0)
  val r1_v4 = (((r1_v3 + (s3_r3_mid << 4)) >> 1).asUInt * "h8E38E39".U)(26, 0)
  val r2_v3 = (s3_r2_mid - r4_final)(26, 0)

  val r3_final = (0.U - s3_r3_mid - r1_v4)(26, 0)

  val r5_final = ((r1_v4 - ((r5_v2 >> 1).asUInt * "hEEEEEEF".U)(26, 0)) >> 1)
  val r1_final = (r1_v4 - r5_final)(26, 0)

  io.valid_out := RegNext(s3_valid_mid, false.B)

  val out_c = Wire(Vec(4, UInt(24.W)))
  out_c(0) := (s3_w6_mid - r2_v3)(23, 0)
  out_c(1) := (r5_final - r1_final)(23, 0)
  out_c(2) := (r4_final - s3_w0_mid)(23, 0)
  out_c(3) := r3_final(23, 0)
  when(io.valid_out) {
    for (i <- 0 until 4) {
      printf("c%d = 0x%x\n", i.U, out_c(i))
    }
  }
  io.c := RegNext(out_c)
}
