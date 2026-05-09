package poly_mult
import chisel3._
import chisel3.util._

class interpIO(
    wWidth: Int = 27,
    outWidth: Int = 24,
    stride: Int = 4
) extends Bundle {
  val valid_in = Input(Bool())
  val w = Input(Vec(7 * stride, UInt(wWidth.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(4 * stride, UInt(outWidth.W)))
}

class interpolation(
    wWidth: Int = 27,
    outWidth: Int = 24,
    stride: Int = 4,
    mk: Int = 27,
    mk2: Int = 24,
    mk3: Int = 25,
    inv3: BigInt = BigInt("AAAAAAB", 16),
    inv9: BigInt = BigInt("E38E39", 16),
    inv18: BigInt = BigInt("EEEEEF", 16)
) extends Module {
  val io = IO(new interpIO(wWidth, outWidth, stride))

  private def mask(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, 0.U), value)
  }

  private val prevR0 = Wire(Vec(stride + 1, UInt(mk2.W)))
  private val prevR1 = Wire(Vec(stride + 1, UInt(mk2.W)))
  private val prevR2 = Wire(Vec(stride + 1, UInt(mk2.W)))
  private val cWire = Wire(Vec(4 * stride, UInt(outWidth.W)))

  prevR0(0) := 0.U
  prevR1(0) := 0.U
  prevR2(0) := 0.U

  for (i <- 0 until stride) {
    val p0 = mask(io.w(i), mk)
    val p1 = mask(io.w(stride + i), mk)
    val p2 = mask(io.w(2 * stride + i), mk)
    val p3 = mask(io.w(3 * stride + i), mk)
    val p4 = mask(io.w(4 * stride + i), mk)
    val p5 = mask(io.w(5 * stride + i), mk)
    val p6 = mask(io.w(6 * stride + i), mk)

    val r1a = mask(p1 + p4, mk)
    val r5a = mask(p5 - p4, mk)
    val r3a = mask(mask(p3 - p2, mk) >> 1, mk)
    val r4a = mask(p4 - p0, mk)
    val r4b = mask((r4a << 1) + r5a - (p6 << 7), mk)
    val r2a = mask(p2 + r3a, mk)
    val r1b = mask(r1a - (r2a << 6) - r2a, mk)
    val r2b = mask(r2a - p6 - p0, mk)
    val r1c = mask(r1b + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), mk)

    val r4d = mask(mask(mask(r4b - (r2b << 3), mk) >> 3, mk) * inv3.U((mk + 6).W), mk2)
    val r5c = mask(mask((r5a + r1c) >> 1, mk) * inv18.U((mk + 6).W), mk3)
    val r1e = mask(mask(mask(r1c + (r3a << 4), mk) >> 1, mk) * inv9.U((mk + 6).W), mk3)

    val r2c = mask(r2b - r4d, mk2)
    val r3b = mask(0.U - r1e - r3a, mk2)
    val r5d = mask((r1e - r5c) >> 1, mk2)
    val r1f = mask(r1e - r5d, mk2)

    cWire(4 * i + 0) := mask(p6 + prevR2(i), outWidth)
    cWire(4 * i + 1) := mask(r5d + prevR1(i), outWidth)
    cWire(4 * i + 2) := mask(r4d + prevR0(i), outWidth)
    cWire(4 * i + 3) := mask(r3b, outWidth)

    prevR0(i + 1) := mask(p0, mk2)
    prevR1(i + 1) := r1f
    prevR2(i + 1) := r2c
  }

  cWire(0) := mask(cWire(0) - prevR2(stride), outWidth)
  cWire(1) := mask(cWire(1) - prevR1(stride), outWidth)
  cWire(2) := mask(cWire(2) - prevR0(stride), outWidth)

  val validReg = RegNext(io.valid_in, false.B)
  val cReg = RegNext(cWire)

  io.valid_out := validReg
  io.c := cReg
}
