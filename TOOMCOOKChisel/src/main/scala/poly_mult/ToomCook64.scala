package poly_mult
import chisel3._
import chisel3.util._

class core64(
    aWidth: Int = 24,
    bWidth: Int = 8,
    aEvalWidth: Int = 39,
    bEvalWidth: Int = 29,
    core16OutWidth: Int = 36,
    outWidth: Int = 24,
    interpMk: Int = 33,
    interpMk2: Int = 30,
    interpMk3: Int = 31,
    interpInv3: BigInt = BigInt("2AAAAAAB", 16),
    interpInv9: BigInt = BigInt("38E38E39", 16),
    interpInv18: BigInt = BigInt("6EEEEEEF", 16)
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(64, UInt(aWidth.W)))
    val b = Input(Vec(64, UInt(bWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(64, UInt(outWidth.W)))
  })

  private def mask(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, 0.U), value)
  }

  private def fillMsb(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
  }

  // Stage 0 : Input register
  val s0_valid = RegNext(io.valid_in, false.B)
  val s0_a = RegNext(io.a)
  val s0_b = RegNext(io.b)

  // Stage 1 : Evaluation over four 16-coefficient blocks.
  // Layout: eval(point * 16 + coefficient)
  val A_eval = Wire(Vec(7 * 16, UInt(aEvalWidth.W)))
  val B_eval = Wire(Vec(7 * 16, UInt(bEvalWidth.W)))

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

    A_eval(j + 0) := mask(ar3, aEvalWidth)
    A_eval(j + 16) := mask(ah2, aEvalWidth)
    A_eval(j + 32) := mask(a_even +& a_odd, aEvalWidth)
    A_eval(j + 48) := fillMsb(a_even -& a_odd, aEvalWidth)
    A_eval(j + 64) := mask(Cat(a_scaled_even, 0.U(1.W)) +& a_scaled_odd, aEvalWidth)
    A_eval(j + 80) := fillMsb(Cat(a_scaled_even, 0.U(1.W)) -& a_scaled_odd, aEvalWidth)
    A_eval(j + 96) := mask(ar0, aEvalWidth)

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

    B_eval(j + 0) := mask(br3, bEvalWidth)
    B_eval(j + 16) := mask(bh2, bEvalWidth)
    B_eval(j + 32) := mask(b_even +& b_odd, bEvalWidth)
    B_eval(j + 48) := fillMsb(b_even -& b_odd, bEvalWidth)
    B_eval(j + 64) := mask(Cat(b_scaled_even, 0.U(1.W)) +& b_scaled_odd, bEvalWidth)
    B_eval(j + 80) := fillMsb(Cat(b_scaled_even, 0.U(1.W)) -& b_scaled_odd, bEvalWidth)
    B_eval(j + 96) := mask(br0, bEvalWidth)
  }

  val s1_valid = RegNext(s0_valid, false.B)
  val s1_A = RegNext(A_eval)
  val s1_B = RegNext(B_eval)

  // Stage 2 : Pointwise 16-coefficient multiplication using seven 1024-width core16 modules.
  val cores = Seq.fill(7)(
    Module(
      new core16(
        aWidth = aEvalWidth,
        bWidth = bEvalWidth,
        aEvalWidth = aEvalWidth,
        bEvalWidth = bEvalWidth,
        core4MulWidth = aEvalWidth,
        core4OutWidth = core16OutWidth,
        outWidth = core16OutWidth,
        interpMk = 36,
        interpMk2 = 33,
        interpMk3 = 34,
        core4Inv3 = BigInt("AAAAAAAAB", 16),
        core4Inv9 = BigInt("E38E38E39", 16),
        core4Inv18 = BigInt("EEEEEEEEF", 16),
        interpInv3 = BigInt("AAAAAAAAB", 16),
        interpInv9 = BigInt("238E38E39", 16),
        interpInv18 = BigInt("2EEEEEEEF", 16)
      )
    )
  )
  for (i <- 0 until 7) {
    cores(i).io.valid_in := s1_valid
    for (j <- 0 until 16) {
      cores(i).io.a(j) := s1_A(i * 16 + j)
      cores(i).io.b(j) := s1_B(i * 16 + j)
    }
  }

  val core_valid = VecInit(cores.map(_.io.valid_out)).asUInt.andR
  val core_c_wire = Wire(Vec(7 * 16, UInt(core16OutWidth.W)))
  for (i <- 0 until 7) {
    for (j <- 0 until 16) {
      core_c_wire(i * 16 + j) := cores(i).io.c(j)
    }
  }

  val s2_valid = RegNext(core_valid, false.B)
  val s2_w = RegNext(core_c_wire)

  // Stage 3 : Vector-wise interpolation from seven 16-coefficient products to 64 coefficients.
  val interp = Module(
    new interpolation(
      wWidth = core16OutWidth,
      outWidth = outWidth,
      stride = 16,
      mk = interpMk,
      mk2 = interpMk2,
      mk3 = interpMk3,
      inv3 = interpInv3,
      inv9 = interpInv9,
      inv18 = interpInv18
    )
  )
  interp.io.valid_in := s2_valid
  interp.io.w := s2_w

  io.valid_out := interp.io.valid_out
  io.c := interp.io.c
}

class ToomCook64 extends core64()
