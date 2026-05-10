package core
import chisel3._
import chisel3.util._
class Interpolation(
    stride: Int,
    wWidth: Int,
    outWidth: Int,
    mk: Int,
    mk2: Int,
    mk3: Int
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val w = Input(Vec(7 * stride, UInt(wWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(4 * stride, UInt(outWidth.W)))
  })

  val interp = Module(new InterpolationComb(stride, wWidth, outWidth, mk, mk2, mk3))
  interp.io.w := io.w

  io.valid_out := RegNext(io.valid_in, false.B)
  io.c := RegNext(interp.io.c)
}

class InterpolationComb(
    stride: Int,
    wWidth: Int,
    outWidth: Int,
    mk: Int,
    mk2: Int,
    mk3: Int
) extends Module {
  val io = IO(new Bundle {
    val w = Input(Vec(7 * stride, UInt(wWidth.W)))
    val c = Output(Vec(4 * stride, UInt(outWidth.W)))
  })

  private val inv3 = MagicNumber.inv3(mk2)
  private val inv9 = MagicNumber.inv9(mk3)
  private val inv15 = MagicNumber.inv15(mk3)

  val cRaw = Wire(Vec(4 * stride, UInt(outWidth.W)))
  val prevR0 = Wire(Vec(stride + 1, UInt(mk2.W)))
  val prevR1 = Wire(Vec(stride + 1, UInt(mk2.W)))
  val prevR2 = Wire(Vec(stride + 1, UInt(mk2.W)))

  prevR0(0) := 0.U
  prevR1(0) := 0.U
  prevR2(0) := 0.U

  for (i <- 0 until stride) {
    val p0 = ParaMath.mask(io.w(i), mk)
    val p1 = ParaMath.mask(io.w(stride + i), mk)
    val p2 = ParaMath.mask(io.w(2 * stride + i), mk)
    val p3 = ParaMath.mask(io.w(3 * stride + i), mk)
    val p4 = ParaMath.mask(io.w(4 * stride + i), mk)
    val p5 = ParaMath.mask(io.w(5 * stride + i), mk)
    val p6 = ParaMath.mask(io.w(6 * stride + i), mk)

    val r1a = ParaMath.mask(p1 + p4, mk)
    val r5a = ParaMath.mask(p5 - p4, mk)
    val r3a = ParaMath.mask(ParaMath.mask(p3 - p2, mk) >> 1, mk)
    val r4a = ParaMath.mask(p4 - p0, mk)
    val r4b = ParaMath.mask((r4a << 1) + r5a - (p6 << 7), mk)
    val r2a = ParaMath.mask(p2 + r3a, mk)
    val r1b = ParaMath.mask(r1a - (r2a << 6) - r2a, mk)
    val r2b = ParaMath.mask(r2a - p6 - p0, mk)
    val r1c = ParaMath.mask(r1b + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), mk)

    val r4d = ParaMath.mask(
      ParaMath.mask(ParaMath.mask(r4b - (r2b << 3), mk) >> 3, mk) * inv3.U,
      mk2
    )
    val r5c = ParaMath.mask(
      ParaMath.mask((r5a + r1c) >> 1, mk) * inv15.U,
      mk3
    )
    val r1e = ParaMath.mask(
      ParaMath.mask(ParaMath.mask(r1c + (r3a << 4), mk) >> 1, mk) * inv9.U,
      mk3
    )

    val r2c = ParaMath.mask(r2b - r4d, mk2)
    val r3b = ParaMath.mask(0.U - r1e - r3a, mk2)
    val r5d = ParaMath.mask((r1e - r5c) >> 1, mk2)
    val r1f = ParaMath.mask(r1e - r5d, mk2)

    cRaw(4 * i + 0) := ParaMath.mask(p6 + prevR2(i), outWidth)
    cRaw(4 * i + 1) := ParaMath.mask(r5d + prevR1(i), outWidth)
    cRaw(4 * i + 2) := ParaMath.mask(r4d + prevR0(i), outWidth)
    cRaw(4 * i + 3) := ParaMath.mask(r3b, outWidth)

    prevR0(i + 1) := ParaMath.mask(p0, mk2)
    prevR1(i + 1) := r1f
    prevR2(i + 1) := r2c
  }

  for (i <- 0 until 4 * stride) {
    io.c(i) := cRaw(i)
  }
  io.c(0) := ParaMath.mask(cRaw(0) - prevR2(stride), outWidth)
  io.c(1) := ParaMath.mask(cRaw(1) - prevR1(stride), outWidth)
  io.c(2) := ParaMath.mask(cRaw(2) - prevR0(stride), outWidth)
}