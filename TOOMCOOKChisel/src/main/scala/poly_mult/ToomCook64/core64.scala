package poly_mult
import chisel3._
import chisel3.util._

class core64 extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(64, UInt(24.W)))
    val b = Input(Vec(64, UInt(8.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(64, UInt(24.W)))
  })

  private def mask(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, 0.U), value)
  }

  private def fillMsb(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
  }

  private val MASK27 = "h7FFFFFF".U
  private val MASK25 = "h1FFFFFF".U
  private val MASK24 = "hFFFFFF".U

  // Stage 0 : Input register
  val s0_valid = RegNext(io.valid_in, false.B)
  val s0_a = RegNext(io.a)
  val s0_b = RegNext(io.b)

  // Stage 1 : Evaluation over four 16-coefficient blocks.
  // Layout: eval(point * 16 + coefficient)
  val A_eval = Wire(Vec(7 * 16, UInt(24.W)))
  val B_eval = Wire(Vec(7 * 16, UInt(8.W)))

  for (j <- 0 until 16) {
    val ar0 = s0_a(j)
    val ar1 = s0_a(16 + j)
    val ar2 = s0_a(32 + j)
    val ar3 = s0_a(48 + j)

    val a_even = ar0 +& ar2
    val a_odd = ar1 +& ar3
    val a_scaled_even = Cat(ar0, 0.U(2.W)) +& ar2
    val a_scaled_odd = Cat(ar1, 0.U(2.W)) +& ar3

    val ah0 = ar2 +& Cat(ar3, 0.U(1.W))
    val ah1 = ar1 +& Cat(ah0, 0.U(1.W))
    val ah2 = ar0 +& Cat(ah1, 0.U(1.W))

    A_eval(j + 0) := ar3
    A_eval(j + 16) := mask(ah2, 24)
    A_eval(j + 32) := mask(a_even +& a_odd, 24)
    A_eval(j + 48) := fillMsb(a_even -& a_odd, 24)
    A_eval(j + 64) := mask(Cat(a_scaled_even, 0.U(1.W)) +& a_scaled_odd, 24)
    A_eval(j + 80) := fillMsb(Cat(a_scaled_even, 0.U(1.W)) -& a_scaled_odd, 24)
    A_eval(j + 96) := ar0

    val br0 = s0_b(j)
    val br1 = s0_b(16 + j)
    val br2 = s0_b(32 + j)
    val br3 = s0_b(48 + j)

    val b_even = br0 +& br2
    val b_odd = br1 +& br3
    val b_scaled_even = Cat(br0, 0.U(2.W)) +& br2
    val b_scaled_odd = Cat(br1, 0.U(2.W)) +& br3

    val bh0 = br2 +& Cat(br3, 0.U(1.W))
    val bh1 = br1 +& Cat(bh0, 0.U(1.W))
    val bh2 = br0 +& Cat(bh1, 0.U(1.W))

    B_eval(j + 0) := br3
    B_eval(j + 16) := mask(bh2, 8)
    B_eval(j + 32) := mask(b_even +& b_odd, 8)
    B_eval(j + 48) := fillMsb(b_even -& b_odd, 8)
    B_eval(j + 64) := mask(Cat(b_scaled_even, 0.U(1.W)) +& b_scaled_odd, 8)
    B_eval(j + 80) := fillMsb(Cat(b_scaled_even, 0.U(1.W)) -& b_scaled_odd, 8)
    B_eval(j + 96) := br0
  }

  val s1_valid = RegNext(s0_valid, false.B)
  val s1_A = RegNext(A_eval)
  val s1_B = RegNext(B_eval)

  // Stage 2 : Pointwise 16-coefficient multiplication using seven core16 modules.
  val cores = Seq.fill(7)(Module(new core16))
  for (i <- 0 until 7) {
    cores(i).io.valid_in := s1_valid
    for (j <- 0 until 16) {
      cores(i).io.a(j) := s1_A(i * 16 + j)
      cores(i).io.b(j) := s1_B(i * 16 + j)
    }
  }

  val core_valid = VecInit(cores.map(_.io.valid_out)).asUInt.andR
  val core_c_wire = Wire(Vec(7 * 16, UInt(24.W)))
  for (i <- 0 until 7) {
    for (j <- 0 until 16) {
      core_c_wire(i * 16 + j) := cores(i).io.c(j)
    }
  }

  val s2_valid = RegNext(core_valid, false.B)
  val s2_w = RegNext(core_c_wire)

  // Stage 3 : Vector-wise interpolation.  This is the same column-by-column
  // interpolation rule used by interpolation.scala, generalized from 4 columns
  // to 16 columns for the four 16-coefficient blocks.
  val r0 = Wire(Vec(16, UInt(27.W)))
  val r1 = Wire(Vec(16, UInt(27.W)))
  val r2 = Wire(Vec(16, UInt(27.W)))
  val r3 = Wire(Vec(16, UInt(27.W)))
  val r4 = Wire(Vec(16, UInt(27.W)))
  val r5 = Wire(Vec(16, UInt(27.W)))
  val r6 = Wire(Vec(16, UInt(27.W)))

  for (j <- 0 until 16) {
    val w0 = mask(s2_w(j), 27)
    val w1 = mask(s2_w(16 + j), 27)
    val w2 = mask(s2_w(32 + j), 27)
    val w3 = mask(s2_w(48 + j), 27)
    val w4 = mask(s2_w(64 + j), 27)
    val w5 = mask(s2_w(80 + j), 27)
    val w6 = mask(s2_w(96 + j), 27)

    val r1a = (w1 + w4) & MASK27
    val r5a = (w5 - w4) & MASK27
    val r3a = ((w3 - w2) >> 1) & MASK27
    val r4a = (w4 - w0) & MASK27
    val r4b = ((r4a << 1) + r5a - (w6 << 7)) & MASK27
    val r2a = (w2 + r3a) & MASK27
    val r1b = (r1a - (r2a << 6) - r2a) & MASK27
    val r2b = (r2a - w6 - w0) & MASK27
    val r1c = (r1b + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5)) & MASK27

    val r4d = ((((r4b - (r2b << 3)) & MASK27) >> 3) * "hAAAAAAB".U) & MASK27
    val r5c = ((((r5a + r1c) >> 1) * "hEEEEEF".U) & MASK25)
    val r1e = (((((r1c + (r3a << 4)) & MASK27) >> 1) * "hE38E39".U) & MASK25)

    val r2c = (r2b - r4d) & MASK24
    val r3b = (0.U - r1e - r3a) & MASK24
    val r5d = (r1e - r5c) >> 1
    val r1f = r1e - r5d

    r0(j) := w0
    r1(j) := r1f
    r2(j) := r2c
    r3(j) := r3b
    r4(j) := r4d
    r5(j) := r5d
    r6(j) := w6
  }

  val c_wire = Wire(Vec(64, UInt(27.W)))
  for (j <- 0 until 16) {
    if (j == 0) {
      c_wire(0) := (r6(0) - r2(15)) & MASK24
      c_wire(1) := (r5(0) - r1(15)) & MASK24
      c_wire(2) := (r4(0) - r0(15)) & MASK24
      c_wire(3) := r3(0)
    } else {
      c_wire(4 * j) := (r6(j) + r2(j - 1)) & MASK24
      c_wire(4 * j + 1) := (r5(j) + r1(j - 1)) & MASK24
      c_wire(4 * j + 2) := (r4(j) + r0(j - 1)) & MASK24
      c_wire(4 * j + 3) := r3(j)
    }
  }

  val s3_valid = RegNext(s2_valid, false.B)
  val s3_c = RegNext(c_wire)

  io.valid_out := s3_valid
  for (i <- 0 until 64) {
    io.c(i) := s3_c(i)(23, 0)
  }
}