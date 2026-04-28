package poly_mult

import chisel3._
import chisel3.util._

// InterpCore：单列插值核心，纯组合硬件模块
class InterpCore(pidx: Int, inW: Int) extends Module {
  private val p   = InterpParamTable.params(pidx)
  private val mk  = p.mk
  private val mk2 = p.mk2
  private val mk3 = p.mk3

  val io = IO(new Bundle {
    val pIn = Input(Vec(7, UInt(inW.W)))
    val pr0 = Input(UInt(mk2.W))
    val pr1 = Input(UInt(mk2.W))
    val pr2 = Input(UInt(mk2.W))

    val c3     = Output(UInt(mk2.W))
    val c0part = Output(UInt(mk2.W))
    val c1part = Output(UInt(mk2.W))
    val c2part = Output(UInt(mk2.W))
    val nr0    = Output(UInt(mk2.W))
    val nr1    = Output(UInt(mk2.W))
    val nr2    = Output(UInt(mk2.W))
  })

  val p0 = BitUtil.mask(io.pIn(0), mk)
  val p1 = BitUtil.mask(io.pIn(1), mk)
  val p2 = BitUtil.mask(io.pIn(2), mk)
  val p3 = BitUtil.mask(io.pIn(3), mk)
  val p4 = BitUtil.mask(io.pIn(4), mk)
  val p5 = BitUtil.mask(io.pIn(5), mk)
  val p6 = BitUtil.mask(io.pIn(6), mk)

  val r5a = BitUtil.mask(p5 - p4, mk)
  val r3a = BitUtil.mask(BitUtil.mask(p3 - p2, mk) >> 1, mk)
  val r4a = BitUtil.mask(p4 - p0, mk)
  val r4b = BitUtil.mask((r4a << 1) + r5a - (p6 << 7), mk)
  val r2a = BitUtil.mask(p2 + r3a, mk)
  val r1a = BitUtil.mask(p1 + p4 - (r2a << 6) - r2a, mk)
  val r2b = BitUtil.mask(r2a - p6 - p0, mk)
  val r1b = BitUtil.mask(r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), mk)

  val r4c = BitUtil.mask(
    BitUtil.mask(BitUtil.mask(r4b - (r2b << 3), mk) >> 3, mk) * p.inv3.U(42.W), mk2
  )
  val r5b = BitUtil.mask(
    BitUtil.mask((r5a + r1b) >> 1, mk) * p.inv18.U(42.W), mk3
  )
  val r1c = BitUtil.mask(
    BitUtil.mask(BitUtil.mask(r1b + (r3a << 4), mk) >> 1, mk) * p.inv9.U(42.W), mk3
  )

  val r2c = BitUtil.mask(r2b - r4c, mk2)
  val r3b = BitUtil.mask(0.U - r3a - r1c, mk2)
  val r5c = BitUtil.mask((r1c - r5b) >> 1, mk2)
  val r1d = BitUtil.mask(r1c - r5c, mk2)

  io.c3     := r3b
  io.c0part := BitUtil.mask(p6 + io.pr2, mk2)
  io.c1part := BitUtil.mask(r5c + io.pr1, mk2)
  io.c2part := BitUtil.mask(r4c + io.pr0, mk2)
  io.nr0    := BitUtil.mask(p0, mk2)
  io.nr1    := r1d
  io.nr2    := r2c
}
