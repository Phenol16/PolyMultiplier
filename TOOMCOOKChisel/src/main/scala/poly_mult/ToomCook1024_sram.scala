package poly_mult_sram

import chisel3._
import chisel3.util._


object Util {
  def mask(value: UInt, targetWidth: Int): UInt = {
    require(targetWidth > 0, "mask width must be positive")
    if (value.getWidth >= targetWidth) {
      value(targetWidth - 1, 0)
    } else {
      Cat(Fill(targetWidth - value.getWidth, 0.U), value)
    }
  }

  def fillMsb(value: UInt, targetWidth: Int): UInt = {
    require(targetWidth > 0, "fillMsb width must be positive")
    if (value.getWidth >= targetWidth) {
      value(targetWidth - 1, 0)
    } else {
      Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
    }
  }
}
import Util._

object TC43EvalWidth {
  val A_EVAL_W = 39
  val B_EVAL_W = 29
}

// =============================================================================
//  插值参数表：编译期常量，不是硬件模块
// =============================================================================
object InterpParamTable {
  case class Param(mk: Int, mk2: Int, mk3: Int,
                   inv3: BigInt, inv9: BigInt, inv18: BigInt)

  val params = Seq(
    // stride=4,   paramIdx=0
    Param(36, 33, 34, BigInt("AAAAAAAAB",  16), BigInt("238E38E39", 16), BigInt("2EEEEEEEF", 16)),
    // stride=16,  paramIdx=1
    Param(33, 30, 31, BigInt("2AAAAAAB",   16), BigInt("38E38E39",  16), BigInt("6EEEEEEF",  16)),
    // stride=64,  paramIdx=2
    Param(30, 27, 28, BigInt("2AAAAAB",    16), BigInt("8E38E39",   16), BigInt("EEEEEEF",   16)),
    // stride=256, paramIdx=3
    Param(27, 24, 25, BigInt("AAAAAB",     16), BigInt("E38E39",    16), BigInt("EEEEEEF",   16))
  )
}

// =============================================================================
//  EvalLayerTC43：4个输入 -> 7个 Toom-Cook 求值点，纯组合硬件模块
// =============================================================================
class EvalLayerTC43(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r   = Input(Vec(4, UInt(inW.W)))
    val out = Output(Vec(7, UInt(outW.W)))
  })

  val r0 = io.r(0)
  val r1 = io.r(1)
  val r2 = io.r(2)
  val r3 = io.r(3)

  val even = r0 +& r2
  val odd  = r1 +& r3

  val scaledEven = Cat(r0, 0.U(2.W)) +& r2
  val scaledOdd  = Cat(r1, 0.U(2.W)) +& r3

  val high0 = r2 +& Cat(r3, 0.U(1.W))
  val high1 = r1 +& Cat(high0, 0.U(1.W))
  val high2 = r0 +& Cat(high1, 0.U(1.W))

  io.out(0) := mask(r3, outW)
  io.out(1) := mask(high2, outW)
  io.out(2) := mask(even +& odd, outW)
  io.out(3) := fillMsb(even -& odd, outW)
  io.out(4) := mask(Cat(scaledEven, 0.U(1.W)) +& scaledOdd, outW)
  io.out(5) := fillMsb(Cat(scaledEven, 0.U(1.W)) -& scaledOdd, outW)
  io.out(6) := mask(r0, outW)
}

// =============================================================================
//  TC4EvalPoint：4个输入按 pt 选择一个求值点，纯组合硬件模块
// =============================================================================
class TC4EvalPoint(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r   = Input(Vec(4, UInt(inW.W)))
    val pt  = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  val r0 = io.r(0)
  val r1 = io.r(1)
  val r2 = io.r(2)
  val r3 = io.r(3)

  val even = r0 +& r2
  val odd  = r1 +& r3

  val scaledEven = Cat(r0, 0.U(2.W)) +& r2
  val scaledOdd  = Cat(r1, 0.U(2.W)) +& r3

  val high0 = r2 +& Cat(r3, 0.U(1.W))
  val high1 = r1 +& Cat(high0, 0.U(1.W))
  val high2 = r0 +& Cat(high1, 0.U(1.W))

  io.out := MuxLookup(io.pt, 0.U(outW.W))(Seq(
    0.U -> mask(r3, outW),
    1.U -> mask(high2, outW),
    2.U -> mask(even +& odd, outW),
    3.U -> fillMsb(even -& odd, outW),
    4.U -> mask(Cat(scaledEven, 0.U(1.W)) +& scaledOdd, outW),
    5.U -> fillMsb(Cat(scaledEven, 0.U(1.W)) -& scaledOdd, outW),
    6.U -> mask(r0, outW)
  ))
}

// =============================================================================
//  BuildEvalVec16：三层 TC4 求值，1024输入 -> 16输出，纯组合硬件模块
//  输入布局：in[64*l + 16*k + 4*j + i]
// =============================================================================
class BuildEvalVec16(memW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Input(Vec(1024, UInt(memW.W)))
    val pt0 = Input(UInt(3.W))
    val pt1 = Input(UInt(3.W))
    val pt2 = Input(UInt(3.W))
    val out = Output(Vec(16, UInt(outW.W)))
  })

  for (l <- 0 until 16) {
    val lv2 = Wire(Vec(4, UInt(outW.W)))

    for (k <- 0 until 4) {
      val lv1 = Wire(Vec(4, UInt(outW.W)))

      for (j <- 0 until 4) {
        val eval0 = Module(new TC4EvalPoint(memW, outW))
        eval0.io.r(0) := io.in(64 * l + 16 * k + 4 * j + 0)
        eval0.io.r(1) := io.in(64 * l + 16 * k + 4 * j + 1)
        eval0.io.r(2) := io.in(64 * l + 16 * k + 4 * j + 2)
        eval0.io.r(3) := io.in(64 * l + 16 * k + 4 * j + 3)
        eval0.io.pt   := io.pt0
        lv1(j)        := eval0.io.out
      }

      val eval1 = Module(new TC4EvalPoint(outW, outW))
      eval1.io.r := lv1
      eval1.io.pt := io.pt1
      lv2(k) := eval1.io.out
    }

    val eval2 = Module(new TC4EvalPoint(outW, outW))
    eval2.io.r := lv2
    eval2.io.pt := io.pt2
    io.out(l) := eval2.io.out
  }
}

// =============================================================================
//  EvalLaneFixed：三层 TC4 求值的固定 lane 版本
//  laneConst 固定后，每拍用 phase 在静态地址集合内选择一个 l
//  避免对 Vec(1024, ...) 使用动态 UInt 下标
// =============================================================================
class EvalLaneFixed(memW: Int, outW: Int, laneConst: Int, evalLanes: Int = 4) extends Module {
  require(evalLanes > 0 && 16 % evalLanes == 0, "evalLanes must be a positive divisor of 16")
  require(laneConst >= 0 && laneConst < evalLanes, "laneConst must be within [0, evalLanes)")
  private val evalPhases = 16 / evalLanes

  val io = IO(new Bundle {
    val in    = Input(Vec(1024, UInt(memW.W)))
    val pt0   = Input(UInt(3.W))
    val pt1   = Input(UInt(3.W))
    val pt2   = Input(UInt(3.W))
    val phase = Input(UInt(log2Ceil(evalPhases).W))
    val out   = Output(UInt(outW.W))
  })

  def pickByPhase(offset: Int): UInt = {
    val defaultIdx = laneConst * 64 + offset
    val tbl = (0 until evalPhases).map { p =>
      val l = laneConst + p * evalLanes
      p.U -> io.in(l * 64 + offset)
    }
    MuxLookup(io.phase, io.in(defaultIdx))(tbl)
  }

  val lv2 = Wire(Vec(4, UInt(outW.W)))

  for (k <- 0 until 4) {
    val lv1 = Wire(Vec(4, UInt(outW.W)))
    for (j <- 0 until 4) {
      val eval0 = Module(new TC4EvalPoint(memW, outW))
      val offset = 16 * k + 4 * j
      eval0.io.r(0) := pickByPhase(offset + 0)
      eval0.io.r(1) := pickByPhase(offset + 1)
      eval0.io.r(2) := pickByPhase(offset + 2)
      eval0.io.r(3) := pickByPhase(offset + 3)
      eval0.io.pt   := io.pt0
      lv1(j)        := eval0.io.out
    }

    val eval1 = Module(new TC4EvalPoint(outW, outW))
    eval1.io.r  := lv1
    eval1.io.pt := io.pt1
    lv2(k)      := eval1.io.out
  }

  val eval2 = Module(new TC4EvalPoint(outW, outW))
  eval2.io.r  := lv2
  eval2.io.pt := io.pt2
  io.out      := eval2.io.out
}

// =============================================================================
//  InterpCoreTC43：单列插值核心，纯组合硬件模块
// =============================================================================
class InterpCoreTC43(pidx: Int, inW: Int) extends Module {
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

  val p0 = mask(io.pIn(0), mk)
  val p1 = mask(io.pIn(1), mk)
  val p2 = mask(io.pIn(2), mk)
  val p3 = mask(io.pIn(3), mk)
  val p4 = mask(io.pIn(4), mk)
  val p5 = mask(io.pIn(5), mk)
  val p6 = mask(io.pIn(6), mk)

  val r5a = mask(p5 - p4, mk)
  val r3a = mask(mask(p3 - p2, mk) >> 1, mk)
  val r4a = mask(p4 - p0, mk)
  val r4b = mask((r4a << 1) + r5a - (p6 << 7), mk)
  val r2a = mask(p2 + r3a, mk)
  val r1a = mask(p1 + p4 - (r2a << 6) - r2a, mk)
  val r2b = mask(r2a - p6 - p0, mk)
  val r1b = mask(r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), mk)

  val r4c = mask(
    mask(mask(r4b - (r2b << 3), mk) >> 3, mk) * p.inv3.U(42.W), mk2
  )
  val r5b = mask(
    mask((r5a + r1b) >> 1, mk) * p.inv18.U(42.W), mk3
  )
  val r1c = mask(
    mask(mask(r1b + (r3a << 4), mk) >> 1, mk) * p.inv9.U(42.W), mk3
  )

  val r2c = mask(r2b - r4c, mk2)
  val r3b = mask(0.U - r3a - r1c, mk2)
  val r5c = mask((r1c - r5b) >> 1, mk2)
  val r1d = mask(r1c - r5c, mk2)

  io.c3     := r3b
  io.c0part := mask(p6 + io.pr2, mk2)
  io.c1part := mask(r5c + io.pr1, mk2)
  io.c2part := mask(r4c + io.pr0, mk2)
  io.nr0    := mask(p0, mk2)
  io.nr1    := r1d
  io.nr2    := r2c
}

// =============================================================================
//  InterpLayerTC43：stride列插值层，纯组合硬件模块
//  wIn 布局：wIn[pt*stride + col]
//  cOut布局：cOut[4*col + k]
// =============================================================================
class InterpLayerTC43(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  private val p   = InterpParamTable.params(pidx)
  private val mk2 = p.mk2

  val io = IO(new Bundle {
    val wIn  = Input(Vec(7 * stride, UInt(inW.W)))
    val cOut = Output(Vec(4 * stride, UInt(outW.W)))
  })

  val cRaw  = Wire(Vec(4 * stride, UInt(outW.W)))
  val prevR0 = Wire(Vec(stride + 1, UInt(mk2.W)))
  val prevR1 = Wire(Vec(stride + 1, UInt(mk2.W)))
  val prevR2 = Wire(Vec(stride + 1, UInt(mk2.W)))

  prevR0(0) := 0.U
  prevR1(0) := 0.U
  prevR2(0) := 0.U

  for (i <- 0 until stride) {
    val core = Module(new InterpCoreTC43(pidx, inW))

    for (pt <- 0 until 7) {
      core.io.pIn(pt) := io.wIn(pt * stride + i)
    }

    core.io.pr0 := prevR0(i)
    core.io.pr1 := prevR1(i)
    core.io.pr2 := prevR2(i)

    cRaw(4 * i + 3) := core.io.c3
    cRaw(4 * i + 0) := core.io.c0part
    cRaw(4 * i + 1) := core.io.c1part
    cRaw(4 * i + 2) := core.io.c2part

    prevR0(i + 1) := core.io.nr0
    prevR1(i + 1) := core.io.nr1
    prevR2(i + 1) := core.io.nr2
  }

  for (i <- 0 until 4 * stride) {
    io.cOut(i) := cRaw(i)
  }

  // 末尾修正：c[0] -= pr2, c[1] -= pr1, c[2] -= pr0
  io.cOut(0) := mask(cRaw(0) - prevR2(stride), outW)
  io.cOut(1) := mask(cRaw(1) - prevR1(stride), outW)
  io.cOut(2) := mask(cRaw(2) - prevR0(stride), outW)
}

// =============================================================================
//  InterpLayerSeqTC43：时序复用插值层
//  仅使用 1 个 InterpCoreTC43，每拍处理一列，总计 stride 列
// =============================================================================
class InterpLayerSeqTC43(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  private val p   = InterpParamTable.params(pidx)
  private val mk2 = p.mk2

  val io = IO(new Bundle {
    val start = Input(Bool())
    val wIn   = Input(Vec(7 * stride, UInt(inW.W)))
    val done  = Output(Bool())
    val cOut  = Output(Vec(4 * stride, UInt(outW.W)))
    val dbg_started = Output(Bool())
    val dbg_ran_any = Output(Bool())
    val dbg_core_any_nonzero = Output(Bool())
    val dbg_c0_before_fix = Output(UInt(outW.W))
    val dbg_c0_after_fix = Output(UInt(outW.W))
  })

  val core = Module(new InterpCoreTC43(pidx, inW))

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

  val dbgStarted = RegInit(false.B)
  val dbgRanAny = RegInit(false.B)
  val dbgCoreAnyNonZero = RegInit(false.B)
  val dbgC0BeforeFix = RegInit(0.U(outW.W))
  val dbgC0AfterFix = RegInit(0.U(outW.W))

  for (pt <- 0 until 7) {
    val row = Wire(Vec(stride, UInt(inW.W)))
    for (i <- 0 until stride) {
      row(i) := io.wIn(pt * stride + i)
    }
    core.io.pIn(pt) := row(colCnt)
  }
  core.io.pr0 := prevR0
  core.io.pr1 := prevR1
  core.io.pr2 := prevR2

  io.done := doneReg
  io.dbg_started := dbgStarted
  io.dbg_ran_any := dbgRanAny
  io.dbg_core_any_nonzero := dbgCoreAnyNonZero
  io.dbg_c0_before_fix := dbgC0BeforeFix
  io.dbg_c0_after_fix := dbgC0AfterFix

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
    dbgStarted := true.B
    dbgRanAny := false.B
    dbgCoreAnyNonZero := false.B
    dbgC0BeforeFix := 0.U
    dbgC0AfterFix := 0.U
    for (i <- 0 until stride) {
      c0Reg(i) := 0.U
      c1Reg(i) := 0.U
      c2Reg(i) := 0.U
      c3Reg(i) := 0.U
    }
  }.elsewhen(running) {
    dbgRanAny := true.B
    when(core.io.c0part.orR || core.io.c1part.orR || core.io.c2part.orR || core.io.c3.orR) {
      dbgCoreAnyNonZero := true.B
    }
    when(colCnt === 0.U) {
      dbgC0BeforeFix := mask(core.io.c0part, outW)
    }
    c0Reg(colCnt) := mask(core.io.c0part, outW)
    c1Reg(colCnt) := mask(core.io.c1part, outW)
    c2Reg(colCnt) := mask(core.io.c2part, outW)
    c3Reg(colCnt) := mask(core.io.c3, outW)

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
    dbgC0AfterFix := mask(c0Reg(0) - prevR2, outW)
    c0Reg(0) := mask(c0Reg(0) - prevR2, outW)
    c1Reg(0) := mask(c1Reg(0) - prevR1, outW)
    c2Reg(0) := mask(c2Reg(0) - prevR0, outW)
    fixStage := false.B
    doneReg := true.B
  }
}

// =============================================================================
//  Product4TC43：4系数 × 4系数 -> 7系数，纯组合硬件模块
// =============================================================================
class Product4TC43 extends Module {
  private val A_EVAL_W = TC43EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC43EvalWidth.B_EVAL_W
  private val PROD_MUL_MOD_W = A_EVAL_W
  private val PROD_OUT_W = 36

  val io = IO(new Bundle {
    val a4  = Input(Vec(4, UInt(A_EVAL_W.W)))
    val b4  = Input(Vec(4, UInt(B_EVAL_W.W)))
    val out = Output(Vec(7, UInt(PROD_OUT_W.W)))
  })

  val evalA = Module(new EvalLayerTC43(A_EVAL_W, A_EVAL_W))
  val evalB = Module(new EvalLayerTC43(B_EVAL_W, B_EVAL_W))

  evalA.io.r := io.a4
  evalB.io.r := io.b4

  val wMul = Wire(Vec(7, UInt(PROD_MUL_MOD_W.W)))

  for (i <- 0 until 7) {
    val bw     = evalB.io.out(i)(B_EVAL_W - 1, 0)
    val bwSign = bw(B_EVAL_W - 1)
    val bwSext = Cat(Fill(A_EVAL_W - B_EVAL_W, bwSign), bw).asSInt
    val awInt  = evalA.io.out(i)(A_EVAL_W - 1, 0).asSInt
    wMul(i) := mask((awInt * bwSext).asUInt, PROD_MUL_MOD_W)
  }

  val r5a = mask(wMul(5) - wMul(4), PROD_MUL_MOD_W)
  val r3a = mask(mask(wMul(3) - wMul(2), PROD_MUL_MOD_W) >> 1, PROD_MUL_MOD_W)
  val r4a = mask(wMul(4) - wMul(0), PROD_MUL_MOD_W)
  val r4b = mask((r4a << 1) + r5a - (wMul(6) << 7), PROD_MUL_MOD_W)
  val r2a = mask(wMul(2) + r3a, PROD_MUL_MOD_W)
  val r1a = mask(wMul(1) + wMul(4) - (r2a << 6) - r2a, PROD_MUL_MOD_W)
  val r2b = mask(r2a - wMul(6) - wMul(0), PROD_MUL_MOD_W)
  val r1b = mask(r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), PROD_MUL_MOD_W)

  val r4c = mask(
    mask(mask(r4b - (r2b << 3), PROD_MUL_MOD_W) >> 3, PROD_MUL_MOD_W) * "hAAAAAAAAB".U(42.W), PROD_OUT_W
  )
  val r5b = mask(
    mask((r5a + r1b) >> 1, PROD_MUL_MOD_W) * "hEEEEEEEEF".U(42.W), 37
  )
  val r1c = mask(
    mask(mask(r1b + (r3a << 4), PROD_MUL_MOD_W) >> 1, PROD_MUL_MOD_W) * "hE38E38E39".U(42.W), 37
  )

  val r2c = mask(r2b - r4c, PROD_OUT_W)
  val r3b = mask(0.U - r3a - r1c, PROD_OUT_W)
  val r5c = mask((r1c - r5b) >> 1, PROD_OUT_W)
  val r1d = mask(r1c - r5c, PROD_OUT_W)

  io.out(0) := mask(wMul(6) - r2c, PROD_OUT_W)
  io.out(1) := mask(r5c - r1d, PROD_OUT_W)
  io.out(2) := mask(r4c - wMul(0), PROD_OUT_W)
  io.out(3) := r3b
  io.out(4) := 0.U
  io.out(5) := 0.U
  io.out(6) := 0.U
}

// =============================================================================
//  Core16TC43：16元素子核
//  模块内部仍保留原设计的一拍寄存器切割：Product4输出 -> InterpLayer输入
// =============================================================================
class Core16TC43 extends Module {
  private val A_EVAL_W = TC43EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC43EvalWidth.B_EVAL_W
  private val CORE_OUT_W = 36

  val io = IO(new Bundle {
    val valid_in  = Input(Bool())
    val avec      = Input(Vec(16, UInt(A_EVAL_W.W)))
    val bvec      = Input(Vec(16, UInt(B_EVAL_W.W)))
    val valid_out = Output(Bool())
    val cOut      = Output(Vec(16, UInt(CORE_OUT_W.W)))
  })

  val ae = Wire(Vec(7 * 4, UInt(A_EVAL_W.W)))
  val be = Wire(Vec(7 * 4, UInt(B_EVAL_W.W)))

  for (seg <- 0 until 4) {
    val evalA = Module(new EvalLayerTC43(A_EVAL_W, A_EVAL_W))
    val evalB = Module(new EvalLayerTC43(B_EVAL_W, B_EVAL_W))

    evalA.io.r(0) := io.avec(seg * 4 + 0)
    evalA.io.r(1) := io.avec(seg * 4 + 1)
    evalA.io.r(2) := io.avec(seg * 4 + 2)
    evalA.io.r(3) := io.avec(seg * 4 + 3)

    evalB.io.r(0) := io.bvec(seg * 4 + 0)
    evalB.io.r(1) := io.bvec(seg * 4 + 1)
    evalB.io.r(2) := io.bvec(seg * 4 + 2)
    evalB.io.r(3) := io.bvec(seg * 4 + 3)

    for (pt <- 0 until 7) {
      ae(pt * 4 + seg) := evalA.io.out(pt)
      be(pt * 4 + seg) := evalB.io.out(pt)
    }
  }

  val wProd = Wire(Vec(7 * 4, UInt(CORE_OUT_W.W)))

  for (pt <- 0 until 7) {
    val prod = Module(new Product4TC43)

    for (k <- 0 until 4) {
      prod.io.a4(k) := ae(pt * 4 + k)
      prod.io.b4(k) := be(pt * 4 + k)
    }

    for (k <- 0 until 4) {
      wProd(pt * 4 + k) := prod.io.out(k)
    }
  }

  val regW     = RegEnable(wProd, io.valid_in)
  val regValid = RegNext(io.valid_in, false.B)

  val interp = Module(new InterpLayerTC43(stride = 4, pidx = 0, inW = 36, outW = 36))
  interp.io.wIn := regW

  io.valid_out := regValid
  io.cOut      := interp.io.cOut
}

// =============================================================================
//  ToomCook43 顶层
//  保留原本的整体流水结构：
//  输入寄存 -> Core16内部寄存 -> W2寄存 -> W1寄存 -> W0寄存 -> 输出寄存
// =============================================================================

class SpRam(width: Int, depth: Int) extends BlackBox(Map("WIDTH" -> width, "DEPTH" -> depth)) with HasBlackBoxResource {
  override def desiredName: String = "sp_ram"
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val en = Input(Bool())
    val we = Input(Bool())
    val addr = Input(UInt(log2Ceil(depth).W))
    val din = Input(UInt(width.W))
    val dout = Output(UInt(width.W))
  })
  addResource("/sp_ram.v")
}

class InterpLayerSeq2ColTC43(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  require(stride % 2 == 0, "2-column interpolation requires even stride")
  private val p = InterpParamTable.params(pidx)
  private val mk2 = p.mk2
  val io = IO(new Bundle {
    val start = Input(Bool())
    val wIn = Input(Vec(7 * stride, UInt(inW.W)))
    val done = Output(Bool())
    val cOut = Output(Vec(4 * stride, UInt(outW.W)))
  })

  val core0 = Module(new InterpCoreTC43(pidx, inW))
  val core1 = Module(new InterpCoreTC43(pidx, inW))
  val colCnt = RegInit(0.U(log2Ceil(stride).W))
  val running = RegInit(false.B)
  val fixStage = RegInit(false.B)
  val doneReg = RegInit(false.B)
  val prevR0 = RegInit(0.U(mk2.W))
  val prevR1 = RegInit(0.U(mk2.W))
  val prevR2 = RegInit(0.U(mk2.W))

  val c0Reg = Reg(Vec(stride, UInt(outW.W)))
  val c1Reg = Reg(Vec(stride, UInt(outW.W)))
  val c2Reg = Reg(Vec(stride, UInt(outW.W)))
  val c3Reg = Reg(Vec(stride, UInt(outW.W)))

  val col1 = colCnt + 1.U
  for (pt <- 0 until 7) {
    val row = Wire(Vec(stride, UInt(inW.W)))
    for (i <- 0 until stride) row(i) := io.wIn(pt * stride + i)
    core0.io.pIn(pt) := row(colCnt)
    core1.io.pIn(pt) := row(col1)
  }

  core0.io.pr0 := prevR0
  core0.io.pr1 := prevR1
  core0.io.pr2 := prevR2
  core1.io.pr0 := core0.io.nr0
  core1.io.pr1 := core0.io.nr1
  core1.io.pr2 := core0.io.nr2

  io.done := doneReg
  for (i <- 0 until stride) {
    io.cOut(4 * i + 0) := c0Reg(i)
    io.cOut(4 * i + 1) := c1Reg(i)
    io.cOut(4 * i + 2) := c2Reg(i)
    io.cOut(4 * i + 3) := c3Reg(i)
  }

  when(doneReg) { doneReg := false.B }

  when(io.start && !running && !fixStage && !doneReg) {
    colCnt := 0.U
    running := true.B
    prevR0 := 0.U; prevR1 := 0.U; prevR2 := 0.U
  }.elsewhen(running) {
    c0Reg(colCnt) := mask(core0.io.c0part, outW)
    c1Reg(colCnt) := mask(core0.io.c1part, outW)
    c2Reg(colCnt) := mask(core0.io.c2part, outW)
    c3Reg(colCnt) := mask(core0.io.c3, outW)
    c0Reg(col1) := mask(core1.io.c0part, outW)
    c1Reg(col1) := mask(core1.io.c1part, outW)
    c2Reg(col1) := mask(core1.io.c2part, outW)
    c3Reg(col1) := mask(core1.io.c3, outW)
    prevR0 := core1.io.nr0
    prevR1 := core1.io.nr1
    prevR2 := core1.io.nr2
    when(colCnt === (stride - 2).U) { running := false.B; fixStage := true.B }
      .otherwise { colCnt := colCnt + 2.U }
  }.elsewhen(fixStage) {
    c0Reg(0) := mask(c0Reg(0) - prevR2, outW)
    c1Reg(0) := mask(c1Reg(0) - prevR1, outW)
    c2Reg(0) := mask(c2Reg(0) - prevR0, outW)
    fixStage := false.B
    doneReg := true.B
  }
}

class ToomCook43IO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(1024, UInt(24.W)))
  val b = Input(Vec(1024, UInt(8.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(1024, UInt(24.W)))
  val dbg_core_nonzero = Output(Bool())
  val dbg_w2_write_nonzero = Output(Bool())
  val dbg_w2_read_nonzero = Output(Bool())
  val dbg_interp16_nonzero = Output(Bool())
  val dbg_w1_write_nonzero = Output(Bool())
  val dbg_w1_read_nonzero = Output(Bool())
  val dbg_interp64_nonzero = Output(Bool())
  val dbg_w0_write_nonzero = Output(Bool())
  val dbg_interp256_nonzero = Output(Bool())
  val dbg_final_nonzero = Output(Bool())
  val dbg_w0_reg_nonzero = Output(Bool())
  val dbg_w0_block_nonzero = Output(UInt(7.W))
  val dbg_interp256_comb_nonzero = Output(Bool())
  val dbg_interp256_comb_c0 = Output(UInt(24.W))
  val dbg_interp256_seq_c0 = Output(UInt(24.W))
  val dbg_i3_seq_started = Output(Bool())
  val dbg_i3_seq_ran_any = Output(Bool())
  val dbg_i3_seq_core_any_nonzero = Output(Bool())
  val dbg_i3_seq_c0_before_fix = Output(UInt(24.W))
  val dbg_i3_seq_c0_after_fix = Output(UInt(24.W))
  val dbg_core_write_count = Output(UInt(16.W))
  val dbg_interp1_group_count = Output(UInt(16.W))
  val dbg_interp2_block_count = Output(UInt(16.W))
  val dbg_i1_state = Output(UInt(4.W))
  val dbg_i2_state = Output(UInt(4.W))
  val dbg_i3_state = Output(UInt(4.W))
  val dbg_w2_ready = Output(UInt(2.W))
  val dbg_w2_empty = Output(UInt(2.W))
  val dbg_w2_reading = Output(UInt(2.W))
  val dbg_w2_writing = Output(UInt(2.W))
  val dbg_w1_block_ready = Output(UInt(2.W))
  val dbg_w0_ready = Output(UInt(7.W))
}

class ToomCook43 extends Module {
  def packVec(xs: Seq[UInt]): UInt = Cat(xs.reverse)
  def unpackVec(x: UInt, n: Int, w: Int): Vec[UInt] = {
    val v = Wire(Vec(n, UInt(w.W)))
    for (i <- 0 until n) v(i) := x((i + 1) * w - 1, i * w)
    v
  }

  val io = IO(new ToomCook43IO)
  private val A_EVAL_W = TC43EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC43EvalWidth.B_EVAL_W
  private val EVAL_LANES = 4

  // Current version keeps the original Vec(1024) input interface and stores
  // the accepted frame in regA/regB.
  // This version only SRAM-izes the Core->Interp W2 and Interp1->Interp2 W1
  // intermediate buffers.
  // External input SRAM + local tile cache is reserved for a later version.
  val regA = Reg(Vec(1024, UInt(24.W)))
  val regB = Reg(Vec(1024, UInt(8.W)))
  val regC = Reg(Vec(1024, UInt(24.W)))
  io.c := regC
  io.valid_out := false.B

  val evalLanesA = (0 until EVAL_LANES).map(l => Module(new EvalLaneFixed(24, A_EVAL_W, l, EVAL_LANES)))
  val evalLanesB = (0 until EVAL_LANES).map(l => Module(new EvalLaneFixed(8, B_EVAL_W, l, EVAL_LANES)))
  val core = Module(new Core16TC43)
  val interp16Seq = Module(new InterpLayerSeqTC43(16, 1, 36, 33))
  val interp64Seq = Module(new InterpLayerSeqTC43(64, 2, 33, 27))
  private val USE_INTERP3_2COL = false
  val interp256Seq = Module(new InterpLayerSeqTC43(256, 3, 27, 24))
  val interp256Comb = Module(new InterpLayerTC43(stride = 256, pidx = 3, inW = 27, outW = 24))
  interp16Seq.io.start := false.B
  interp64Seq.io.start := false.B
  interp256Seq.io.start := false.B

  val busy = RegInit(false.B)
  val dbgCoreNonZero = RegInit(false.B)
  val dbgW2WriteNonZero = RegInit(false.B)
  val dbgW2ReadNonZero = RegInit(false.B)
  val dbgInterp16NonZero = RegInit(false.B)
  val dbgW1WriteNonZero = RegInit(false.B)
  val dbgW1ReadNonZero = RegInit(false.B)
  val dbgInterp64NonZero = RegInit(false.B)
  val dbgW0WriteNonZero = RegInit(false.B)
  val dbgInterp256NonZero = RegInit(false.B)
  val dbgFinalNonZero = RegInit(false.B)
  val dbgW0RegNonZero = RegInit(false.B)
  val dbgW0BlockNonZero = RegInit(VecInit(Seq.fill(7)(false.B)))
  val dbgInterp256CombNonZero = RegInit(false.B)
  val dbgInterp256CombC0 = RegInit(0.U(24.W))
  val dbgInterp256SeqC0 = RegInit(0.U(24.W))
  val dbgCoreWriteCount = RegInit(0.U(16.W))
  val dbgInterp1Count = RegInit(0.U(16.W))
  val dbgInterp2Count = RegInit(0.U(16.W))
  val evalPhase = RegInit(0.U(2.W))
  val pt0 = RegInit(0.U(3.W)); val pt1 = RegInit(0.U(3.W)); val pt2 = RegInit(0.U(3.W))
  val evalDone = RegInit(false.B)

  val fifoValid = RegInit(false.B)
  val fifoAvec = Reg(Vec(16, UInt(A_EVAL_W.W)))
  val fifoBvec = Reg(Vec(16, UInt(B_EVAL_W.W)))
  val fifoPt0 = Reg(UInt(3.W)); val fifoPt1 = Reg(UInt(3.W)); val fifoPt2 = Reg(UInt(3.W))
  val avecBuild = Reg(Vec(16, UInt(A_EVAL_W.W)))
  val bvecBuild = Reg(Vec(16, UInt(B_EVAL_W.W)))

  val corePending = RegInit(false.B)
  val outPt0 = Reg(UInt(3.W)); val outPt1 = Reg(UInt(3.W)); val outPt2 = Reg(UInt(3.W))
  val w2WBuf = RegInit(0.U(1.W))

  val w2Ram = Seq.fill(2, 7)(Module(new SpRam(576, 2)))
  val w1Ram = Seq.fill(2, 7)(Module(new SpRam(2112, 2)))
  val w2Empty = RegInit(VecInit(Seq.fill(2)(true.B)))
  val w2Writing = RegInit(VecInit(Seq.fill(2)(false.B)))
  val w2Reading = RegInit(VecInit(Seq.fill(2)(false.B)))
  val w2Ready = RegInit(VecInit(Seq.fill(2)(false.B)))
  val w2Full = RegInit(VecInit(Seq.fill(2) { VecInit(Seq.fill(7)(false.B)) }))
  val w2Pt0 = Reg(Vec(2, UInt(3.W))); val w2Pt1 = Reg(Vec(2, UInt(3.W)))

  val w1BufValid = RegInit(VecInit(Seq.fill(2)(false.B)))
  val w1BufBlock = Reg(Vec(2, UInt(3.W)))
  val w1SubReady = RegInit(VecInit(Seq.fill(2) { VecInit(Seq.fill(7)(false.B)) }))
  val w1BlockReady = RegInit(VecInit(Seq.fill(2)(false.B)))

  val w0Reg = Reg(Vec(7, Vec(256, UInt(27.W))))
  val w0Ready = RegInit(VecInit(Seq.fill(7)(false.B)))

  val i1Idle :: i1ReadReq :: i1ReadCap :: i1Start :: i1Run :: i1WriteW1 :: Nil = Enum(6)
  val i2Idle :: i2ReadReq :: i2ReadCap :: i2Start :: i2Run :: i2WriteW0 :: Nil = Enum(6)
  val i3Idle :: i3Start :: i3Run :: i3OutValid :: Nil = Enum(4)
  val i1State = RegInit(i1Idle)
  val i2State = RegInit(i2Idle)
  val i3State = RegInit(i3Idle)
  val i1Buf = RegInit(0.U(1.W))
  val i2Buf = RegInit(0.U(1.W))

  val w2Local = Reg(Vec(7, Vec(16, UInt(36.W))))
  val w1Local = Reg(Vec(7, Vec(64, UInt(33.W))))
  for (i <- 0 until 7 * 16) interp16Seq.io.wIn(i) := w2Local(i / 16)(i % 16)
  for (i <- 0 until 7 * 64) interp64Seq.io.wIn(i) := w1Local(i / 64)(i % 64)
  for (g <- 0 until 7; k <- 0 until 256) {
    interp256Seq.io.wIn(g * 256 + k) := w0Reg(g)(k)
    interp256Comb.io.wIn(g * 256 + k) := w0Reg(g)(k)
  }

  for (b <- 0 until 2; p <- 0 until 7) {
    w2Ram(b)(p).io.clk := clock; w2Ram(b)(p).io.en := false.B; w2Ram(b)(p).io.we := false.B; w2Ram(b)(p).io.addr := 0.U(1.W); w2Ram(b)(p).io.din := 0.U
    w1Ram(b)(p).io.clk := clock; w1Ram(b)(p).io.en := false.B; w1Ram(b)(p).io.we := false.B; w1Ram(b)(p).io.addr := 0.U(1.W); w1Ram(b)(p).io.din := 0.U
  }

  for (l <- 0 until EVAL_LANES) {
    evalLanesA(l).io.in := regA; evalLanesA(l).io.pt0 := pt0; evalLanesA(l).io.pt1 := pt1; evalLanesA(l).io.pt2 := pt2; evalLanesA(l).io.phase := evalPhase
    evalLanesB(l).io.in := regB; evalLanesB(l).io.pt0 := pt0; evalLanesB(l).io.pt1 := pt1; evalLanesB(l).io.pt2 := pt2; evalLanesB(l).io.phase := evalPhase
  }

  val canPush = busy && !fifoValid && !evalDone
  val nextAvec = Wire(Vec(16, UInt(A_EVAL_W.W)))
  val nextBvec = Wire(Vec(16, UInt(B_EVAL_W.W)))
  nextAvec := avecBuild
  nextBvec := bvecBuild
  for (l <- 0 until EVAL_LANES) {
    val idx = evalPhase * EVAL_LANES.U + l.U
    nextAvec(idx) := evalLanesA(l).io.out
    nextBvec(idx) := evalLanesB(l).io.out
  }

  when(canPush) {
    avecBuild := nextAvec
    bvecBuild := nextBvec
    when(evalPhase === 3.U) {
      fifoValid := true.B
      fifoAvec := nextAvec
      fifoBvec := nextBvec
      fifoPt0 := pt0; fifoPt1 := pt1; fifoPt2 := pt2
      evalPhase := 0.U
      when(pt0 === 6.U && pt1 === 6.U && pt2 === 6.U) { evalDone := true.B }
        .otherwise {
          when(pt2 === 6.U) {
            pt2 := 0.U
            when(pt1 === 6.U) { pt1 := 0.U; pt0 := pt0 + 1.U }
              .otherwise { pt1 := pt1 + 1.U }
          }.otherwise { pt2 := pt2 + 1.U }
        }
    }.otherwise { evalPhase := evalPhase + 1.U }
  }

  core.io.valid_in := false.B
  core.io.avec := fifoAvec
  core.io.bvec := fifoBvec
  val canUseW2WriteBuf = (w2Empty(w2WBuf) || w2Writing(w2WBuf)) && !w2Reading(w2WBuf) && !w2Ready(w2WBuf)
  when(busy && fifoValid && !corePending && canUseW2WriteBuf) {
    core.io.valid_in := true.B
    corePending := true.B
    w2Empty(w2WBuf) := false.B
    w2Writing(w2WBuf) := true.B
    outPt0 := fifoPt0; outPt1 := fifoPt1; outPt2 := fifoPt2
    fifoValid := false.B
  }

  for (buf <- 0 until 2; p <- 0 until 7) {
    when(corePending && core.io.valid_out && w2WBuf === buf.U && outPt2 === p.U) {
      w2Ram(buf)(p).io.en := true.B
      w2Ram(buf)(p).io.we := true.B
      w2Ram(buf)(p).io.din := packVec(core.io.cOut)
    }
  }
  when(corePending && core.io.valid_out) {
    dbgCoreWriteCount := dbgCoreWriteCount + 1.U
    when(core.io.cOut.asUInt.orR) {
      dbgCoreNonZero := true.B
      dbgW2WriteNonZero := true.B
    }
    corePending := false.B
    when(w2WBuf === 0.U) {
      val next0 = Wire(Vec(7, Bool()))
      next0 := w2Full(0)
      next0(outPt2) := true.B
      w2Full(0) := next0
      when(next0.asUInt.andR) {
        w2Writing(0) := false.B
        w2Ready(0) := true.B
        w2Pt0(0) := outPt0
        w2Pt1(0) := outPt1
        w2WBuf := 1.U
      }
    }.otherwise {
      val next1 = Wire(Vec(7, Bool()))
      next1 := w2Full(1)
      next1(outPt2) := true.B
      w2Full(1) := next1
      when(next1.asUInt.andR) {
        w2Writing(1) := false.B
        w2Ready(1) := true.B
        w2Pt0(1) := outPt0
        w2Pt1(1) := outPt1
        w2WBuf := 0.U
      }
    }
  }

  when(i1State === i1Idle) {
    when(w2Ready(0) && !w2Writing(0)) { i1Buf := 0.U; w2Reading(0) := true.B; i1State := i1ReadReq }
      .elsewhen(w2Ready(1) && !w2Writing(1)) { i1Buf := 1.U; w2Reading(1) := true.B; i1State := i1ReadReq }
  }.elsewhen(i1State === i1ReadReq) {
    i1State := i1ReadCap
  }.elsewhen(i1State === i1ReadCap) {
    val w2ReadAnyNonZero = WireDefault(false.B)
    for (p <- 0 until 7) {
      val d = Mux(i1Buf === 0.U, w2Ram(0)(p).io.dout, w2Ram(1)(p).io.dout)
      when(d.orR) { w2ReadAnyNonZero := true.B }
      w2Local(p) := unpackVec(d, 16, 36)
    }
    when(w2ReadAnyNonZero) { dbgW2ReadNonZero := true.B }
    i1State := i1Start
  }.elsewhen(i1State === i1Start) {
    interp16Seq.io.start := true.B
    i1State := i1Run
  }.elsewhen(i1State === i1Run) {
    when(interp16Seq.io.done) { i1State := i1WriteW1 }
  }.elsewhen(i1State === i1WriteW1) {
    when(interp16Seq.io.cOut.asUInt.orR) {
      dbgInterp16NonZero := true.B
      dbgW1WriteNonZero := true.B
    }
    val curBlock = Mux(i1Buf === 0.U, w2Pt0(0), w2Pt0(1))
    val curSub = Mux(i1Buf === 0.U, w2Pt1(0), w2Pt1(1))
    val hit0 = w1BufValid(0) && (w1BufBlock(0) === curBlock)
    val hit1 = w1BufValid(1) && (w1BufBlock(1) === curBlock)
    val empty0 = !w1BufValid(0)
    val empty1 = !w1BufValid(1)
    val canAlloc = hit0 || hit1 || empty0 || empty1
    when(canAlloc) {
      val selBuf = Wire(UInt(1.W))
      selBuf := Mux(hit0 || (!hit1 && empty0), 0.U, 1.U)
      when(selBuf === 0.U) {
        when(!w1BufValid(0)) { w1BufValid(0) := true.B; w1BufBlock(0) := curBlock }
      }.otherwise {
        when(!w1BufValid(1)) { w1BufValid(1) := true.B; w1BufBlock(1) := curBlock }
      }
      for (buf <- 0 until 2; sub <- 0 until 7) {
        when(selBuf === buf.U && curSub === sub.U) {
          w1Ram(buf)(sub).io.en := true.B
          w1Ram(buf)(sub).io.we := true.B
          w1Ram(buf)(sub).io.din := packVec(interp16Seq.io.cOut)
        }
      }
      val oldReady = Wire(Vec(7, Bool()))
      oldReady := Mux(selBuf === 0.U, w1SubReady(0), w1SubReady(1))
      val nextReady = Wire(Vec(7, Bool()))
      nextReady := oldReady
      nextReady(curSub) := true.B
      when(selBuf === 0.U) {
        w1SubReady(0) := nextReady
        when(nextReady.asUInt.andR) { w1BlockReady(0) := true.B }
      }.otherwise {
        w1SubReady(1) := nextReady
        when(nextReady.asUInt.andR) { w1BlockReady(1) := true.B }
      }
      w2Ready(i1Buf) := false.B
      w2Reading(i1Buf) := false.B
      w2Empty(i1Buf) := true.B
      w2Full(i1Buf) := VecInit(Seq.fill(7)(false.B))
      dbgInterp1Count := dbgInterp1Count + 1.U
      i1State := i1Idle
    }
  }

  for (buf <- 0 until 2) {
    when(i1Buf === buf.U && i1State === i1ReadReq) {
      for (p <- 0 until 7) { w2Ram(buf)(p).io.en := true.B; w2Ram(buf)(p).io.we := false.B }
    }
    when(i2Buf === buf.U && i2State === i2ReadReq) {
      for (s <- 0 until 7) { w1Ram(buf)(s).io.en := true.B; w1Ram(buf)(s).io.we := false.B }
    }
  }

  when(i2State === i2Idle) {
    when(w1BlockReady(0)) { i2Buf := 0.U; i2State := i2ReadReq }
      .elsewhen(w1BlockReady(1)) { i2Buf := 1.U; i2State := i2ReadReq }
  }.elsewhen(i2State === i2ReadReq) {
    i2State := i2ReadCap
  }.elsewhen(i2State === i2ReadCap) {
    val w1ReadAnyNonZero = WireDefault(false.B)
    for (s <- 0 until 7) {
      val d = Mux(i2Buf === 0.U, w1Ram(0)(s).io.dout, w1Ram(1)(s).io.dout)
      when(d.orR) { w1ReadAnyNonZero := true.B }
      w1Local(s) := unpackVec(d, 64, 33)
    }
    when(w1ReadAnyNonZero) { dbgW1ReadNonZero := true.B }
    i2State := i2Start
  }.elsewhen(i2State === i2Start) {
    interp64Seq.io.start := true.B
    i2State := i2Run
  }.elsewhen(i2State === i2Run) {
    when(interp64Seq.io.done) { i2State := i2WriteW0 }
  }.elsewhen(i2State === i2WriteW0) {
    val blk = Mux(i2Buf === 0.U, w1BufBlock(0), w1BufBlock(1))
    when(interp64Seq.io.cOut.asUInt.orR) {
      dbgInterp64NonZero := true.B
      dbgW0WriteNonZero := true.B
      dbgW0RegNonZero := true.B
    }
    when(blk === 0.U && interp64Seq.io.cOut.asUInt.orR) { dbgW0BlockNonZero(0) := true.B }
    when(blk === 1.U && interp64Seq.io.cOut.asUInt.orR) { dbgW0BlockNonZero(1) := true.B }
    when(blk === 2.U && interp64Seq.io.cOut.asUInt.orR) { dbgW0BlockNonZero(2) := true.B }
    when(blk === 3.U && interp64Seq.io.cOut.asUInt.orR) { dbgW0BlockNonZero(3) := true.B }
    when(blk === 4.U && interp64Seq.io.cOut.asUInt.orR) { dbgW0BlockNonZero(4) := true.B }
    when(blk === 5.U && interp64Seq.io.cOut.asUInt.orR) { dbgW0BlockNonZero(5) := true.B }
    when(blk === 6.U && interp64Seq.io.cOut.asUInt.orR) { dbgW0BlockNonZero(6) := true.B }
    for (i <- 0 until 256) w0Reg(blk)(i) := interp64Seq.io.cOut(i)
    w0Ready(blk) := true.B
    w1BlockReady(i2Buf) := false.B
    w1SubReady(i2Buf) := VecInit(Seq.fill(7)(false.B))
    w1BufValid(i2Buf) := false.B
    dbgInterp2Count := dbgInterp2Count + 1.U
    i2State := i2Idle
  }

  val w0RegAnyNonZero = WireDefault(false.B)
  val w0BlockNZ = Wire(Vec(7, Bool()))
  for (g <- 0 until 7) {
    w0BlockNZ(g) := w0Reg(g).asUInt.orR
    when(w0Reg(g).asUInt.orR) {
      w0RegAnyNonZero := true.B
    }
  }

  when(i3State === i3Idle && busy && w0Ready.asUInt.andR) {
    i3State := i3Start
  }.elsewhen(i3State === i3Start) {
    when(w0RegAnyNonZero) {
      dbgW0RegNonZero := true.B
    }
    dbgW0BlockNonZero := w0BlockNZ
    when(interp256Comb.io.cOut.asUInt.orR) {
      dbgInterp256CombNonZero := true.B
    }
    dbgInterp256CombC0 := interp256Comb.io.cOut(0)
    interp256Seq.io.start := true.B
    i3State := i3Run
  }.elsewhen(i3State === i3Run && interp256Seq.io.done) {
    dbgInterp256SeqC0 := interp256Seq.io.cOut(0)
    when(interp256Seq.io.cOut.asUInt.orR) {
      dbgInterp256NonZero := true.B
      dbgFinalNonZero := true.B
    }
    for (i <- 0 until 1024) regC(i) := mask(interp256Seq.io.cOut(i), 24)
    i3State := i3OutValid
  }.elsewhen(i3State === i3OutValid) {
    io.valid_out := true.B
    busy := false.B
    i3State := i3Idle
  }

  // valid_in is accepted only when busy=false.
  // The caller/testbench must not assert valid_in while busy=true.
  // Future version may add ready_in for streaming multi-frame input.
  when(io.valid_in && !busy) {
    regA := io.a
    regB := io.b
    busy := true.B
    pt0 := 0.U; pt1 := 0.U; pt2 := 0.U; evalPhase := 0.U; evalDone := false.B
    fifoValid := false.B; corePending := false.B
    w2Empty := VecInit(Seq.fill(2)(true.B))
    w2Writing := VecInit(Seq.fill(2)(false.B))
    w2Reading := VecInit(Seq.fill(2)(false.B))
    w2Ready := VecInit(Seq.fill(2)(false.B))
    w2Full := VecInit(Seq.fill(2) { VecInit(Seq.fill(7)(false.B)) })
    w1BlockReady := VecInit(Seq.fill(2)(false.B))
    w1SubReady := VecInit(Seq.fill(2) { VecInit(Seq.fill(7)(false.B)) })
    w1BufValid := VecInit(Seq.fill(2)(false.B))
    w0Ready := VecInit(Seq.fill(7)(false.B))
    i1State := i1Idle; i2State := i2Idle; i3State := i3Idle
    w2WBuf := 0.U
    dbgCoreNonZero := false.B
    dbgW2WriteNonZero := false.B
    dbgW2ReadNonZero := false.B
    dbgInterp16NonZero := false.B
    dbgW1WriteNonZero := false.B
    dbgW1ReadNonZero := false.B
    dbgInterp64NonZero := false.B
    dbgW0WriteNonZero := false.B
    dbgInterp256NonZero := false.B
    dbgFinalNonZero := false.B
    dbgCoreWriteCount := 0.U
    dbgInterp1Count := 0.U
    dbgInterp2Count := 0.U
    dbgW0RegNonZero := false.B
    dbgW0BlockNonZero := VecInit(Seq.fill(7)(false.B))
    dbgInterp256CombNonZero := false.B
    dbgInterp256CombC0 := 0.U
    dbgInterp256SeqC0 := 0.U
  }

  io.dbg_core_nonzero := dbgCoreNonZero
  io.dbg_w2_write_nonzero := dbgW2WriteNonZero
  io.dbg_w2_read_nonzero := dbgW2ReadNonZero
  io.dbg_interp16_nonzero := dbgInterp16NonZero
  io.dbg_w1_write_nonzero := dbgW1WriteNonZero
  io.dbg_w1_read_nonzero := dbgW1ReadNonZero
  io.dbg_interp64_nonzero := dbgInterp64NonZero
  io.dbg_w0_write_nonzero := dbgW0WriteNonZero
  io.dbg_interp256_nonzero := dbgInterp256NonZero
  io.dbg_final_nonzero := dbgFinalNonZero
  io.dbg_w0_reg_nonzero := dbgW0RegNonZero
  io.dbg_w0_block_nonzero := dbgW0BlockNonZero.asUInt
  io.dbg_interp256_comb_nonzero := dbgInterp256CombNonZero
  io.dbg_interp256_comb_c0 := dbgInterp256CombC0
  io.dbg_interp256_seq_c0 := dbgInterp256SeqC0
  io.dbg_i3_seq_started := interp256Seq.io.dbg_started
  io.dbg_i3_seq_ran_any := interp256Seq.io.dbg_ran_any
  io.dbg_i3_seq_core_any_nonzero := interp256Seq.io.dbg_core_any_nonzero
  io.dbg_i3_seq_c0_before_fix := interp256Seq.io.dbg_c0_before_fix
  io.dbg_i3_seq_c0_after_fix := interp256Seq.io.dbg_c0_after_fix
  io.dbg_core_write_count := dbgCoreWriteCount
  io.dbg_interp1_group_count := dbgInterp1Count
  io.dbg_interp2_block_count := dbgInterp2Count
  io.dbg_i1_state := i1State.asUInt
  io.dbg_i2_state := i2State.asUInt
  io.dbg_i3_state := i3State.asUInt
  io.dbg_w2_ready := w2Ready.asUInt
  io.dbg_w2_empty := w2Empty.asUInt
  io.dbg_w2_reading := w2Reading.asUInt
  io.dbg_w2_writing := w2Writing.asUInt
  io.dbg_w1_block_ready := w1BlockReady.asUInt
  io.dbg_w0_ready := w0Ready.asUInt
}