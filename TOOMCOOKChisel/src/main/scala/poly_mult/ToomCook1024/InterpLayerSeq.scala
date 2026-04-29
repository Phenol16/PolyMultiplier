package poly_mult

import chisel3._
import chisel3.util._

// InterpLayerSeq：时序复用插值层
// 仅使用 1 个 InterpCore，每拍处理一列，总计 stride 列
class InterpLayerSeq(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  private val p   = InterpParamTable.params(pidx)
  private val mk2 = p.mk2

  val io = IO(new Bundle {
    val start = Input(Bool())
    val wIn   = Input(Vec(7 * stride, UInt(inW.W)))
    val done  = Output(Bool())
    val cOut  = Output(Vec(4 * stride, UInt(outW.W)))
  })

  val core = Module(new InterpCore(pidx, inW))

  val colCnt   = RegInit(0.U(log2Ceil(stride).W))
  val running  = RegInit(false.B)
  val fixStage = RegInit(false.B)
  val doneReg  = RegInit(false.B)

  val prevR0 = RegInit(0.U(mk2.W))
  val prevR1 = RegInit(0.U(mk2.W))
  val prevR2 = RegInit(0.U(mk2.W))

  val c0Reg = Reg(Vec(stride, UInt(outW.W)))
  val c1Reg = Reg(Vec(stride, UInt(outW.W)))
  val c2Reg = Reg(Vec(stride, UInt(outW.W)))
  val c3Reg = Reg(Vec(stride, UInt(outW.W)))

  for (pt <- 0 until 7) {
    val rdIdx = (pt * stride).U + colCnt
    core.io.pIn(pt) := io.wIn(rdIdx)
  }
  core.io.pr0 := prevR0
  core.io.pr1 := prevR1
  core.io.pr2 := prevR2

  io.done := doneReg

  for (i <- 0 until stride) {
    io.cOut(4 * i + 0) := c0Reg(i)
    io.cOut(4 * i + 1) := c1Reg(i)
    io.cOut(4 * i + 2) := c2Reg(i)
    io.cOut(4 * i + 3) := c3Reg(i)
  }

  when(doneReg) {
    doneReg := false.B
  }

  when(io.start && !running && !fixStage && !doneReg) {
    colCnt   := 0.U
    running  := true.B
    fixStage := false.B
    prevR0   := 0.U
    prevR1   := 0.U
    prevR2   := 0.U
  }.elsewhen(running) {
    c0Reg(colCnt) := BitUtil.mask(core.io.c0part, outW)
    c1Reg(colCnt) := BitUtil.mask(core.io.c1part, outW)
    c2Reg(colCnt) := BitUtil.mask(core.io.c2part, outW)
    c3Reg(colCnt) := BitUtil.mask(core.io.c3, outW)

    prevR0 := core.io.nr0
    prevR1 := core.io.nr1
    prevR2 := core.io.nr2

    when(colCnt === (stride - 1).U) {
      running  := false.B
      fixStage := true.B
    }.otherwise {
      colCnt := colCnt + 1.U
    }
  }.elsewhen(fixStage) {
    // 末尾修正：c[0] -= pr2, c[1] -= pr1, c[2] -= pr0
    c0Reg(0) := BitUtil.mask(c0Reg(0) - prevR2, outW)
    c1Reg(0) := BitUtil.mask(c1Reg(0) - prevR1, outW)
    c2Reg(0) := BitUtil.mask(c2Reg(0) - prevR0, outW)
    fixStage := false.B
    doneReg := true.B
  }
}
