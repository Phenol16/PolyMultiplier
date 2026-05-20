package High

import chisel3._
import chisel3.util._
import core._

class Interp16ColsStep(pidx: Int, inW: Int, outW: Int) extends Module {
  private val mk2 = InterpParamTable.params(pidx).mk2

  val io = IO(new Bundle {
    val in = Input(Vec(7 * 16, UInt(inW.W)))
    val pr0 = Input(UInt(mk2.W))
    val pr1 = Input(UInt(mk2.W))
    val pr2 = Input(UInt(mk2.W))
    val out = Output(Vec(16 * 4, UInt(outW.W)))
    val nr0 = Output(UInt(mk2.W))
    val nr1 = Output(UInt(mk2.W))
    val nr2 = Output(UInt(mk2.W))
  })

  val carry0 = Wire(Vec(17, UInt(mk2.W)))
  val carry1 = Wire(Vec(17, UInt(mk2.W)))
  val carry2 = Wire(Vec(17, UInt(mk2.W)))
  carry0(0) := io.pr0
  carry1(0) := io.pr1
  carry2(0) := io.pr2

  for (col <- 0 until 16) {
    val core = Module(new InterpStepCore(pidx, inW))
    for (pt <- 0 until 7) core.io.pIn(pt) := io.in(pt * 16 + col)
    core.io.pr0 := carry0(col)
    core.io.pr1 := carry1(col)
    core.io.pr2 := carry2(col)

    io.out(col * 4 + 0) := ParaMath.mask(core.io.c0part, outW)
    io.out(col * 4 + 1) := ParaMath.mask(core.io.c1part, outW)
    io.out(col * 4 + 2) := ParaMath.mask(core.io.c2part, outW)
    io.out(col * 4 + 3) := ParaMath.mask(core.io.c3, outW)
    carry0(col + 1) := core.io.nr0
    carry1(col + 1) := core.io.nr1
    carry2(col + 1) := core.io.nr2
  }

  io.nr0 := carry0(16)
  io.nr1 := carry1(16)
  io.nr2 := carry2(16)
}

class InterpStepCore(pidx: Int, inW: Int) extends Module {
  private val p = InterpParamTable.params(pidx)
  private val mk = p.mk
  private val mk2 = p.mk2
  private val mk3 = p.mk3

  val io = IO(new Bundle {
    val pIn = Input(Vec(7, UInt(inW.W)))
    val pr0 = Input(UInt(mk2.W))
    val pr1 = Input(UInt(mk2.W))
    val pr2 = Input(UInt(mk2.W))
    val c3 = Output(UInt(mk2.W))
    val c0part = Output(UInt(mk2.W))
    val c1part = Output(UInt(mk2.W))
    val c2part = Output(UInt(mk2.W))
    val nr0 = Output(UInt(mk2.W))
    val nr1 = Output(UInt(mk2.W))
    val nr2 = Output(UInt(mk2.W))
  })

  val p0 = ParaMath.mask(io.pIn(0), mk)
  val p1 = ParaMath.mask(io.pIn(1), mk)
  val p2 = ParaMath.mask(io.pIn(2), mk)
  val p3 = ParaMath.mask(io.pIn(3), mk)
  val p4 = ParaMath.mask(io.pIn(4), mk)
  val p5 = ParaMath.mask(io.pIn(5), mk)
  val p6 = ParaMath.mask(io.pIn(6), mk)

  val r5a = ParaMath.mask(p5 - p4, mk)
  val r3a = ParaMath.mask(ParaMath.mask(p3 - p2, mk) >> 1, mk)
  val r4a = ParaMath.mask(p4 - p0, mk)
  val r4b = ParaMath.mask((r4a << 1) + r5a - (p6 << 7), mk)
  val r2a = ParaMath.mask(p2 + r3a, mk)
  val r1a = ParaMath.mask(p1 + p4 - (r2a << 6) - r2a, mk)
  val r2b = ParaMath.mask(r2a - p6 - p0, mk)
  val r1b = ParaMath.mask(r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), mk)
  val r4c = ParaMath.mask(ParaMath.mask(ParaMath.mask(r4b - (r2b << 3), mk) >> 3, mk) * p.inv3.U(42.W), mk2)
  val r5b = ParaMath.mask(ParaMath.mask((r5a + r1b) >> 1, mk) * p.inv15.U(42.W), mk3)
  val r1c = ParaMath.mask(ParaMath.mask(ParaMath.mask(r1b + (r3a << 4), mk) >> 1, mk) * p.inv9.U(42.W), mk3)
  val r2c = ParaMath.mask(r2b - r4c, mk2)
  val r3b = ParaMath.mask(0.U - r3a - r1c, mk2)
  val r5c = ParaMath.mask((r1c - r5b) >> 1, mk2)
  val r1d = ParaMath.mask(r1c - r5c, mk2)

  io.c3 := r3b
  io.c0part := ParaMath.mask(p6 + io.pr2, mk2)
  io.c1part := ParaMath.mask(r5c + io.pr1, mk2)
  io.c2part := ParaMath.mask(r4c + io.pr0, mk2)
  io.nr0 := ParaMath.mask(p0, mk2)
  io.nr1 := r1d
  io.nr2 := r2c
}