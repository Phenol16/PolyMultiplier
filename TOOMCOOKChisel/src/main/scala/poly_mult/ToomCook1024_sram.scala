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

object TC4EvalWidth {
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
//  EvalLayerTC4：4个输入 -> 7个 Toom-Cook 求值点，纯组合硬件模块
// =============================================================================
class EvalLayerTC4(inW: Int, outW: Int) extends Module {
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
//  InterpCoreTC4：单列插值核心，纯组合硬件模块
// =============================================================================
class InterpCoreTC4(pidx: Int, inW: Int) extends Module {
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
//  InterpLayerTC4：stride列插值层，纯组合硬件模块
//  wIn 布局：wIn[pt*stride + col]
//  cOut布局：cOut[4*col + k]
// =============================================================================
class InterpLayerTC4(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
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
    val core = Module(new InterpCoreTC4(pidx, inW))

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
//  InterpLayerSeqTC4：时序复用插值层
//  仅使用 1 个 InterpCoreTC4，每拍处理一列，总计 stride 列
// =============================================================================
class InterpLayerSeqTC4(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
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

  val core = Module(new InterpCoreTC4(pidx, inW))

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
  // Debug 端口保留以兼容既有测试接口，但默认不再为其分配寄存器。
  // 插值结果只在 done 后被消费，运行过程中的 debug 历史寄存没有功能必要。
  io.dbg_started := false.B
  io.dbg_ran_any := false.B
  io.dbg_core_any_nonzero := false.B
  io.dbg_c0_before_fix := 0.U
  io.dbg_c0_after_fix := 0.U

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
    // c0/c1/c2/c3 在一次 run 内会被完整覆盖；done 前不会读取，
    // 因此 start 时不清零，避免生成大规模无意义写入 mux 和翻转。
  }.elsewhen(running) {
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
    c0Reg(0) := mask(c0Reg(0) - prevR2, outW)
    c1Reg(0) := mask(c1Reg(0) - prevR1, outW)
    c2Reg(0) := mask(c2Reg(0) - prevR0, outW)
    fixStage := false.B
    doneReg := true.B
  }
}
class InterpLayerSeq2ColTC4(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  require(stride % 2 == 0, "2-column interpolation requires even stride")
  private val p = InterpParamTable.params(pidx)
  private val mk2 = p.mk2
  val io = IO(new Bundle {
    val start = Input(Bool())
    val wIn = Input(Vec(7 * stride, UInt(inW.W)))
    val done = Output(Bool())
    val cOut = Output(Vec(4 * stride, UInt(outW.W)))
  })

  val core0 = Module(new InterpCoreTC4(pidx, inW))
  val core1 = Module(new InterpCoreTC4(pidx, inW))
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
    // c0/c1/c2/c3 在每次 run 中被完整覆盖，done 前不被使用，start 时无需清零。
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



// =============================================================================
//  InterpLayerSeqStreamTC4：单列流式输出版本
//  每拍复用 1 个 InterpCoreTC4 处理一列；普通列立即输出 4 个系数。
//  第 0 列需要等待末尾 prevR 修正，因此只用 4 个小寄存器暂存，不再分配
//  stride 深度的 c0/c1/c2/c3 结果缓存。
// =============================================================================
class InterpLayerSeqStreamTC4(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  private val p = InterpParamTable.params(pidx)
  private val mk2 = p.mk2

  val io = IO(new Bundle {
    val start = Input(Bool())
    val wIn = Input(Vec(7 * stride, UInt(inW.W)))
    val outValid = Output(Bool())
    val outBase = Output(UInt(log2Ceil(4 * stride).W))
    val outData = Output(Vec(4, UInt(outW.W)))
    val done = Output(Bool())
  })

  val core = Module(new InterpCoreTC4(pidx, inW))
  val colCnt = RegInit(0.U(log2Ceil(stride).W))
  val running = RegInit(false.B)
  val fixStage = RegInit(false.B)

  val prevR0 = RegInit(0.U(mk2.W))
  val prevR1 = RegInit(0.U(mk2.W))
  val prevR2 = RegInit(0.U(mk2.W))
  val firstC0 = Reg(UInt(outW.W))
  val firstC1 = Reg(UInt(outW.W))
  val firstC2 = Reg(UInt(outW.W))
  val firstC3 = Reg(UInt(outW.W))

  for (pt <- 0 until 7) {
    val row = Wire(Vec(stride, UInt(inW.W)))
    for (i <- 0 until stride) row(i) := io.wIn(pt * stride + i)
    core.io.pIn(pt) := row(colCnt)
  }
  core.io.pr0 := prevR0
  core.io.pr1 := prevR1
  core.io.pr2 := prevR2

  io.outValid := false.B
  io.outBase := 0.U
  io.outData(0) := 0.U
  io.outData(1) := 0.U
  io.outData(2) := 0.U
  io.outData(3) := 0.U
  io.done := false.B

  when(io.start && !running && !fixStage) {
    colCnt := 0.U
    running := true.B
    fixStage := false.B
    prevR0 := 0.U
    prevR1 := 0.U
    prevR2 := 0.U
  }.elsewhen(running) {
    when(colCnt === 0.U) {
      firstC0 := mask(core.io.c0part, outW)
      firstC1 := mask(core.io.c1part, outW)
      firstC2 := mask(core.io.c2part, outW)
      firstC3 := mask(core.io.c3, outW)
    }.otherwise {
      io.outValid := true.B
      io.outBase := (colCnt << 2).asUInt
      io.outData(0) := mask(core.io.c0part, outW)
      io.outData(1) := mask(core.io.c1part, outW)
      io.outData(2) := mask(core.io.c2part, outW)
      io.outData(3) := mask(core.io.c3, outW)
    }

    prevR0 := core.io.nr0
    prevR1 := core.io.nr1
    prevR2 := core.io.nr2
    when(colCnt === (stride - 1).U) {
      running := false.B
      fixStage := true.B
    }.otherwise {
      colCnt := colCnt + 1.U
    }
  }.elsewhen(fixStage) {
    io.outValid := true.B
    io.outBase := 0.U
    io.outData(0) := mask(firstC0 - prevR2, outW)
    io.outData(1) := mask(firstC1 - prevR1, outW)
    io.outData(2) := mask(firstC2 - prevR0, outW)
    io.outData(3) := firstC3
    io.done := true.B
    fixStage := false.B
  }
}





// =============================================================================
//  InterpLayerSeqStreamInOutTC4：输入/输出都按列流式的插值层
//  上游每拍提供当前列的 7 个 point；本模块只保留第 0 列修正所需的
//  4 个小寄存器，不再需要 stride 深度输入缓存或输出缓存。
// =============================================================================
class InterpLayerSeqStreamInOutTC4(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  private val p = InterpParamTable.params(pidx)
  private val mk2 = p.mk2

  val io = IO(new Bundle {
    val start = Input(Bool())
    val inValid = Input(Bool())
    val inData = Input(Vec(7, UInt(inW.W)))
    val inReady = Output(Bool())
    val outValid = Output(Bool())
    val outBase = Output(UInt(log2Ceil(4 * stride).W))
    val outData = Output(Vec(4, UInt(outW.W)))
    val done = Output(Bool())
  })

  val core = Module(new InterpCoreTC4(pidx, inW))
  val colCnt = RegInit(0.U(log2Ceil(stride).W))
  val running = RegInit(false.B)
  val fixStage = RegInit(false.B)

  val prevR0 = RegInit(0.U(mk2.W))
  val prevR1 = RegInit(0.U(mk2.W))
  val prevR2 = RegInit(0.U(mk2.W))
  val firstC0 = Reg(UInt(outW.W))
  val firstC1 = Reg(UInt(outW.W))
  val firstC2 = Reg(UInt(outW.W))
  val firstC3 = Reg(UInt(outW.W))

  for (pt <- 0 until 7) core.io.pIn(pt) := io.inData(pt)
  core.io.pr0 := prevR0
  core.io.pr1 := prevR1
  core.io.pr2 := prevR2

  io.inReady := running
  io.outValid := false.B
  io.outBase := 0.U
  io.outData(0) := 0.U
  io.outData(1) := 0.U
  io.outData(2) := 0.U
  io.outData(3) := 0.U
  io.done := false.B

  when(io.start && !running && !fixStage) {
    colCnt := 0.U
    running := true.B
    fixStage := false.B
    prevR0 := 0.U
    prevR1 := 0.U
    prevR2 := 0.U
  }.elsewhen(running && io.inValid) {
    when(colCnt === 0.U) {
      firstC0 := mask(core.io.c0part, outW)
      firstC1 := mask(core.io.c1part, outW)
      firstC2 := mask(core.io.c2part, outW)
      firstC3 := mask(core.io.c3, outW)
    }.otherwise {
      io.outValid := true.B
      io.outBase := (colCnt << 2).asUInt
      io.outData(0) := mask(core.io.c0part, outW)
      io.outData(1) := mask(core.io.c1part, outW)
      io.outData(2) := mask(core.io.c2part, outW)
      io.outData(3) := mask(core.io.c3, outW)
    }

    prevR0 := core.io.nr0
    prevR1 := core.io.nr1
    prevR2 := core.io.nr2
    when(colCnt === (stride - 1).U) {
      running := false.B
      fixStage := true.B
    }.otherwise {
      colCnt := colCnt + 1.U
    }
  }.elsewhen(fixStage) {
    io.outValid := true.B
    io.outBase := 0.U
    io.outData(0) := mask(firstC0 - prevR2, outW)
    io.outData(1) := mask(firstC1 - prevR1, outW)
    io.outData(2) := mask(firstC2 - prevR0, outW)
    io.outData(3) := firstC3
    io.done := true.B
    fixStage := false.B
  }
}

// =============================================================================
//  InterpLayerSeq2ColStreamInTC4：两列/拍、流式输入版本
//  final stage 直接从 w0 SRAM 每拍读 7 个 54-bit pair 后喂入本模块，
//  消除顶层 w0Reg(Vec(7, Vec(256, ...)))。输出缓存仍保留在模块内部，
//  便于不改变 ToomCook43.io.c 的 Vec(1024, UInt(24.W)) 接口。
// =============================================================================
class InterpLayerSeq2ColStreamInTC4(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  require(stride % 2 == 0, "2-column streaming interpolation requires even stride")
  private val p = InterpParamTable.params(pidx)
  private val mk2 = p.mk2

  val io = IO(new Bundle {
    val start = Input(Bool())
    val inValid = Input(Bool())
    val inPair = Input(Vec(7, UInt((2 * inW).W)))
    val inReady = Output(Bool())
    val done = Output(Bool())
    val cOut = Output(Vec(4 * stride, UInt(outW.W)))
  })

  val core0 = Module(new InterpCoreTC4(pidx, inW))
  val core1 = Module(new InterpCoreTC4(pidx, inW))
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

  for (pt <- 0 until 7) {
    val even = io.inPair(pt)(inW - 1, 0)
    val odd = io.inPair(pt)(2 * inW - 1, inW)
    core0.io.pIn(pt) := even
    core1.io.pIn(pt) := odd
  }
  core0.io.pr0 := prevR0
  core0.io.pr1 := prevR1
  core0.io.pr2 := prevR2
  core1.io.pr0 := core0.io.nr0
  core1.io.pr1 := core0.io.nr1
  core1.io.pr2 := core0.io.nr2

  io.inReady := running
  io.done := doneReg
  when(doneReg) { doneReg := false.B }

  for (i <- 0 until stride) {
    io.cOut(4 * i + 0) := c0Reg(i)
    io.cOut(4 * i + 1) := c1Reg(i)
    io.cOut(4 * i + 2) := c2Reg(i)
    io.cOut(4 * i + 3) := c3Reg(i)
  }

  when(io.start && !running && !fixStage && !doneReg) {
    colCnt := 0.U
    running := true.B
    prevR0 := 0.U
    prevR1 := 0.U
    prevR2 := 0.U
  }.elsewhen(running && io.inValid) {
    val col1 = colCnt + 1.U
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
    when(colCnt === (stride - 2).U) {
      running := false.B
      fixStage := true.B
    }.otherwise {
      colCnt := colCnt + 2.U
    }
  }.elsewhen(fixStage) {
    c0Reg(0) := mask(c0Reg(0) - prevR2, outW)
    c1Reg(0) := mask(c1Reg(0) - prevR1, outW)
    c2Reg(0) := mask(c2Reg(0) - prevR0, outW)
    fixStage := false.B
    doneReg := true.B
  }
}

// =============================================================================
//  Product4TC4：4系数 × 4系数 -> 7系数，纯组合硬件模块
// =============================================================================
class Product4TC4 extends Module {
  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W
  private val PROD_MUL_MOD_W = A_EVAL_W
  private val PROD_OUT_W = 36

  val io = IO(new Bundle {
    val a4  = Input(Vec(4, UInt(A_EVAL_W.W)))
    val b4  = Input(Vec(4, UInt(B_EVAL_W.W)))
    val out = Output(Vec(7, UInt(PROD_OUT_W.W)))
  })

  val evalA = Module(new EvalLayerTC4(A_EVAL_W, A_EVAL_W))
  val evalB = Module(new EvalLayerTC4(B_EVAL_W, B_EVAL_W))

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
//  Core16TC4：16元素子核
//  模块内部仍保留原设计的一拍寄存器切割：Product4输出 -> InterpLayer输入
// =============================================================================
class Core16TC4 extends Module {
  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W
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
    val evalA = Module(new EvalLayerTC4(A_EVAL_W, A_EVAL_W))
    val evalB = Module(new EvalLayerTC4(B_EVAL_W, B_EVAL_W))

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
    val prod = Module(new Product4TC4)

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

  val interp = Module(new InterpLayerTC4(stride = 4, pidx = 0, inW = 36, outW = 36))
  interp.io.wIn := regW

  io.valid_out := regValid
  io.cOut      := interp.io.cOut
}

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

class EvalCoreJob(aW: Int, bW: Int) extends Bundle {
  val avec = Vec(16, UInt(aW.W))
  val bvec = Vec(16, UInt(bW.W))
  val pt0 = UInt(3.W)
  val pt1 = UInt(3.W)
  val pt2 = UInt(3.W)
}
// =============================================================================
//  ToomCook43 顶层
//  保留原本的整体流水结构：
//  输入寄存 -> Core16内部寄存 -> W2寄存 -> W1寄存 -> W0寄存 -> 输出寄存
// =============================================================================
class ToomCook43IO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(1024, UInt(24.W)))
  val b = Input(Vec(1024, UInt(8.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(1024, UInt(24.W)))
  val dbg_core_write_count = Output(UInt(16.W))
  val dbg_interp1_group_count = Output(UInt(16.W))
  val dbg_interp2_block_count = Output(UInt(16.W))
}


class ToomCook43 extends Module {
  def packVec(xs: Seq[UInt]): UInt = Cat(xs.reverse)
  def unpackVec(x: UInt, n: Int, w: Int): Vec[UInt] = {
    val v = Wire(Vec(n, UInt(w.W)))
    for (i <- 0 until n) v(i) := x((i + 1) * w - 1, i * w)
    v
  }

  val io = IO(new ToomCook43IO)
  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W
  private val EVAL_LANES = 4

  val regA = Reg(Vec(1024, UInt(24.W)))
  val regB = Reg(Vec(1024, UInt(8.W)))
  io.valid_out := false.B

  val evalLanesA = (0 until EVAL_LANES).map(l => Module(new EvalLaneFixed(24, A_EVAL_W, l, EVAL_LANES)))
  val evalLanesB = (0 until EVAL_LANES).map(l => Module(new EvalLaneFixed(8, B_EVAL_W, l, EVAL_LANES)))
  val core = Module(new Core16TC4)
  val interp16Seq = Module(new InterpLayerSeqStreamInOutTC4(16, 1, 36, 33))
  val interp64Seq = Module(new InterpLayerSeqStreamInOutTC4(64, 2, 33, 27))
  val interp256Seq = Module(new InterpLayerSeq2ColStreamInTC4(256, 3, 27, 24))
  interp16Seq.io.start := false.B
  interp64Seq.io.start := false.B
  interp256Seq.io.start := false.B

  val busy = RegInit(false.B)
  val dbgCoreWriteCount = RegInit(0.U(16.W))
  val dbgInterp1Count = RegInit(0.U(16.W))
  val dbgInterp2Count = RegInit(0.U(16.W))
  val evalPhase = RegInit(0.U(2.W))
  val pt0 = RegInit(0.U(3.W)); val pt1 = RegInit(0.U(3.W)); val pt2 = RegInit(0.U(3.W))
  val evalDone = RegInit(false.B)

  val evalCoreQ = Module(new Queue(new EvalCoreJob(A_EVAL_W, B_EVAL_W), 2, pipe = true, flow = false))
  val avecBuild = Reg(Vec(16, UInt(A_EVAL_W.W)))
  val bvecBuild = Reg(Vec(16, UInt(B_EVAL_W.W)))

  val corePending = RegInit(false.B)
  val outPt0 = Reg(UInt(3.W)); val outPt1 = Reg(UInt(3.W)); val outPt2 = Reg(UInt(3.W))
  val w2WBuf = RegInit(0.U(1.W))

  val w2Ram = Seq.fill(2, 7)(Module(new SpRam(144, 4)))
  val w1Ram = Seq.fill(2, 7)(Module(new SpRam(132, 16)))
  // w0 使用 7 个单端口 SRAM bank，每个地址保存一整列 4 个 27-bit 系数：Cat(c3,c2,c1,c0)。
  // SRAM 总 bit 数与 54x128 相同，但 interp64 每列只需一拍写入，恢复写入吞吐。
  val w0Ram = Seq.fill(7)(Module(new SpRam(108, 64)))
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

  val w0Ready = RegInit(VecInit(Seq.fill(7)(false.B)))

  val i1Idle :: i1Start :: i1Run :: Nil = Enum(3)
  val i2Idle :: i2Start :: i2Run :: Nil = Enum(3)
  val i3Idle :: i3Start :: i3ReadPipe :: i3Run :: Nil = Enum(4)
  val i1State = RegInit(i1Idle)
  val i2State = RegInit(i2Idle)
  val i3State = RegInit(i3Idle)
  val i1Buf = RegInit(0.U(1.W))
  val i2Buf = RegInit(0.U(1.W))
  val w2WritePending = RegInit(false.B)
  val w2WriteGroup = RegInit(0.U(2.W))
  val w2WriteBuf = RegInit(0.U(1.W))
  val w2WritePt2 = RegInit(0.U(3.W))
  val w2WritePt0 = RegInit(0.U(3.W))
  val w2WritePt1 = RegInit(0.U(3.W))
  val w2WriteData = Reg(Vec(16, UInt(36.W)))
  val w2ReadCol = RegInit(0.U(5.W))
  val w2ReadFeedValid = RegInit(false.B)
  val w2ReadFeedSel = RegInit(0.U(2.W))
  val w2ReadFeedCount = RegInit(0.U(5.W))
  val w1WriteBuf = RegInit(0.U(1.W))
  val w1WriteSub = RegInit(0.U(3.W))
  val w1ReadCol = RegInit(0.U(7.W))
  val w1ReadFeedValid = RegInit(false.B)
  val w1ReadFeedSel = RegInit(0.U(2.W))
  val w1ReadFeedCount = RegInit(0.U(7.W))
  val w0WriteBlock = RegInit(0.U(3.W))
  val w0ReadPairIdx = RegInit(0.U(8.W))
  val w0ReadFeedValid = RegInit(false.B)
  val w0ReadFeedSel = RegInit(false.B)

  interp16Seq.io.inValid := false.B
  for (p <- 0 until 7) interp16Seq.io.inData(p) := 0.U
  interp64Seq.io.inValid := false.B
  for (s <- 0 until 7) interp64Seq.io.inData(s) := 0.U
  interp256Seq.io.inValid := false.B
  for (g <- 0 until 7) interp256Seq.io.inPair(g) := 0.U
  for (i <- 0 until 1024) io.c(i) := mask(interp256Seq.io.cOut(i), 24)

  for (b <- 0 until 2; p <- 0 until 7) {
    w2Ram(b)(p).io.clk := clock; w2Ram(b)(p).io.en := false.B; w2Ram(b)(p).io.we := false.B; w2Ram(b)(p).io.addr := 0.U(2.W); w2Ram(b)(p).io.din := 0.U
    w1Ram(b)(p).io.clk := clock; w1Ram(b)(p).io.en := false.B; w1Ram(b)(p).io.we := false.B; w1Ram(b)(p).io.addr := 0.U(4.W); w1Ram(b)(p).io.din := 0.U
  }
  for (g <- 0 until 7) {
    w0Ram(g).io.clk := clock; w0Ram(g).io.en := false.B; w0Ram(g).io.we := false.B; w0Ram(g).io.addr := 0.U(6.W); w0Ram(g).io.din := 0.U
  }

  for (l <- 0 until EVAL_LANES) {
    evalLanesA(l).io.in := regA; evalLanesA(l).io.pt0 := pt0; evalLanesA(l).io.pt1 := pt1; evalLanesA(l).io.pt2 := pt2; evalLanesA(l).io.phase := evalPhase
    evalLanesB(l).io.in := regB; evalLanesB(l).io.pt0 := pt0; evalLanesB(l).io.pt1 := pt1; evalLanesB(l).io.pt2 := pt2; evalLanesB(l).io.phase := evalPhase
  }

  val nextAvec = Wire(Vec(16, UInt(A_EVAL_W.W)))
  val nextBvec = Wire(Vec(16, UInt(B_EVAL_W.W)))
  nextAvec := avecBuild
  nextBvec := bvecBuild
  for (l <- 0 until EVAL_LANES) {
    val idx = evalPhase * EVAL_LANES.U + l.U
    nextAvec(idx) := evalLanesA(l).io.out
    nextBvec(idx) := evalLanesB(l).io.out
  }

  val evalAtLastPhase = evalPhase === 3.U
  val evalJobValid = busy && !evalDone && evalAtLastPhase
  val evalNonLastStep = busy && !evalDone && !evalAtLastPhase

  evalCoreQ.io.enq.valid := evalJobValid
  evalCoreQ.io.enq.bits.avec := nextAvec
  evalCoreQ.io.enq.bits.bvec := nextBvec
  evalCoreQ.io.enq.bits.pt0 := pt0
  evalCoreQ.io.enq.bits.pt1 := pt1
  evalCoreQ.io.enq.bits.pt2 := pt2
  evalCoreQ.io.deq.ready := false.B

  when(evalNonLastStep) {
    avecBuild := nextAvec
    bvecBuild := nextBvec
    evalPhase := evalPhase + 1.U
  }

  when(evalJobValid && evalCoreQ.io.enq.fire) {
    avecBuild := nextAvec
    bvecBuild := nextBvec
    evalPhase := 0.U
    when(pt0 === 6.U && pt1 === 6.U && pt2 === 6.U) { evalDone := true.B }
      .otherwise {
        when(pt2 === 6.U) {
          pt2 := 0.U
          when(pt1 === 6.U) { pt1 := 0.U; pt0 := pt0 + 1.U }
            .otherwise { pt1 := pt1 + 1.U }
        }.otherwise { pt2 := pt2 + 1.U }
      }
  }

  core.io.valid_in := false.B
  core.io.avec := evalCoreQ.io.deq.bits.avec
  core.io.bvec := evalCoreQ.io.deq.bits.bvec
  val canUseW2WriteBuf = (w2Empty(w2WBuf) || w2Writing(w2WBuf)) && !w2Reading(w2WBuf) && !w2Ready(w2WBuf)
  val canCoreTake = busy && evalCoreQ.io.deq.valid && !corePending && !w2WritePending && canUseW2WriteBuf
  evalCoreQ.io.deq.ready := canCoreTake
  when(canCoreTake) {
    core.io.valid_in := true.B
    corePending := true.B
    w2Empty(w2WBuf) := false.B
    w2Writing(w2WBuf) := true.B
    outPt0 := evalCoreQ.io.deq.bits.pt0
    outPt1 := evalCoreQ.io.deq.bits.pt1
    outPt2 := evalCoreQ.io.deq.bits.pt2
  }

  when(corePending && core.io.valid_out) {
    dbgCoreWriteCount := dbgCoreWriteCount + 1.U
    corePending := false.B
    w2WritePending := true.B
    w2WriteGroup := 0.U
    w2WriteBuf := w2WBuf
    w2WritePt0 := outPt0
    w2WritePt1 := outPt1
    w2WritePt2 := outPt2
    w2WriteData := core.io.cOut
  }

  when(w2WritePending) {
    assert(w2WriteGroup <= 3.U, "w2WriteGroup must stay within grouped W2 SRAM depth")
    assert(!((w2WriteBuf === 0.U && w2Reading(0)) || (w2WriteBuf === 1.U && w2Reading(1))),
      "W2 grouped SRAM must not read and write the same buffer in one cycle")
    val base = Cat(w2WriteGroup, 0.U(2.W))
    for (buf <- 0 until 2; p <- 0 until 7) {
      when(w2WriteBuf === buf.U && w2WritePt2 === p.U) {
        w2Ram(buf)(p).io.en := true.B
        w2Ram(buf)(p).io.we := true.B
        w2Ram(buf)(p).io.addr := w2WriteGroup
        w2Ram(buf)(p).io.din := Cat(
          w2WriteData(base + 3.U), w2WriteData(base + 2.U),
          w2WriteData(base + 1.U), w2WriteData(base + 0.U)
        )
      }
    }
    when(w2WriteGroup === 3.U) {
      w2WritePending := false.B
      when(w2WriteBuf === 0.U) {
        val next0 = Wire(Vec(7, Bool()))
        next0 := w2Full(0)
        next0(w2WritePt2) := true.B
        w2Full(0) := next0
        when(next0.asUInt.andR) {
          w2Writing(0) := false.B
          w2Ready(0) := true.B
          w2Pt0(0) := w2WritePt0
          w2Pt1(0) := w2WritePt1
          w2WBuf := 1.U
        }
      }.otherwise {
        val next1 = Wire(Vec(7, Bool()))
        next1 := w2Full(1)
        next1(w2WritePt2) := true.B
        w2Full(1) := next1
        when(next1.asUInt.andR) {
          w2Writing(1) := false.B
          w2Ready(1) := true.B
          w2Pt0(1) := w2WritePt0
          w2Pt1(1) := w2WritePt1
          w2WBuf := 0.U
        }
      }
    }.otherwise {
      w2WriteGroup := w2WriteGroup + 1.U
    }
  }

  when(i1State === i1Idle) {
    when(w2Ready(0) && !w2Writing(0)) { i1Buf := 0.U; w2Reading(0) := true.B; i1State := i1Start }
      .elsewhen(w2Ready(1) && !w2Writing(1)) { i1Buf := 1.U; w2Reading(1) := true.B; i1State := i1Start }
  }.elsewhen(i1State === i1Start) {
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
      w1WriteBuf := selBuf
      w1WriteSub := curSub
      when(selBuf === 0.U) {
        when(!w1BufValid(0)) { w1BufValid(0) := true.B; w1BufBlock(0) := curBlock }
      }.otherwise {
        when(!w1BufValid(1)) { w1BufValid(1) := true.B; w1BufBlock(1) := curBlock }
      }
      interp16Seq.io.start := true.B
      // start 同周期发起 W2 col=0 的同步读，下一拍喂给 stream-in interp16。
      for (buf <- 0 until 2; p <- 0 until 7) {
        when(i1Buf === buf.U) {
          w2Ram(buf)(p).io.en := true.B
          w2Ram(buf)(p).io.we := false.B
          w2Ram(buf)(p).io.addr := 0.U(2.W)
        }
      }
      w2ReadCol := 1.U
      w2ReadFeedValid := true.B
      w2ReadFeedSel := 0.U
      w2ReadFeedCount := 0.U
      i1State := i1Run
    }
  }.elsewhen(i1State === i1Run) {
    assert(w2ReadCol <= 16.U, "W2 stream read column must not exceed 16")
    when(w2ReadFeedValid) {
      assert(interp16Seq.io.inReady, "interp16Seq must be ready whenever W2 stream drives inValid")
      interp16Seq.io.inValid := true.B
      for (p <- 0 until 7) {
        val word = Mux(i1Buf === 0.U, w2Ram(0)(p).io.dout, w2Ram(1)(p).io.dout)
        interp16Seq.io.inData(p) := MuxLookup(w2ReadFeedSel, word(35, 0))(Seq(
          0.U -> word(35, 0),
          1.U -> word(71, 36),
          2.U -> word(107, 72),
          3.U -> word(143, 108)
        ))
      }
      w2ReadFeedCount := w2ReadFeedCount + 1.U
    }
    when(w2ReadCol === 16.U) {
      w2ReadFeedValid := false.B
    }.otherwise {
      for (buf <- 0 until 2; p <- 0 until 7) {
        when(i1Buf === buf.U) {
          w2Ram(buf)(p).io.en := true.B
          w2Ram(buf)(p).io.we := false.B
          w2Ram(buf)(p).io.addr := (w2ReadCol >> 2)(1, 0)
        }
      }
      w2ReadFeedSel := w2ReadCol(1, 0)
      w2ReadCol := w2ReadCol + 1.U
    }
    when(interp16Seq.io.outValid) {
      for (buf <- 0 until 2; sub <- 0 until 7) {
        when(w1WriteBuf === buf.U && w1WriteSub === sub.U) {
          w1Ram(buf)(sub).io.en := true.B
          w1Ram(buf)(sub).io.we := true.B
          // W1 每个地址保存 interp16 输出的一整列 4 个 33-bit 系数，消除 w1Local 大寄存器。
          w1Ram(buf)(sub).io.addr := (interp16Seq.io.outBase >> 2)(3, 0)
          w1Ram(buf)(sub).io.din := Cat(
            interp16Seq.io.outData(3), interp16Seq.io.outData(2),
            interp16Seq.io.outData(1), interp16Seq.io.outData(0)
          )
        }
      }
    }
    when(interp16Seq.io.done) {
      assert(w2ReadFeedCount === 16.U, "interp16 W2 stream should feed exactly 16 columns")
      val oldReady = Wire(Vec(7, Bool()))
      oldReady := Mux(w1WriteBuf === 0.U, w1SubReady(0), w1SubReady(1))
      val nextReady = Wire(Vec(7, Bool()))
      nextReady := oldReady
      nextReady(w1WriteSub) := true.B
      when(w1WriteBuf === 0.U) {
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
      w2ReadFeedValid := false.B
      dbgInterp1Count := dbgInterp1Count + 1.U
      i1State := i1Idle
    }
  }

  when(i2State === i2Idle) {
    when(w1BlockReady(0)) { i2Buf := 0.U; i2State := i2Start }
      .elsewhen(w1BlockReady(1)) { i2Buf := 1.U; i2State := i2Start }
  }.elsewhen(i2State === i2Start) {
    interp64Seq.io.start := true.B
    w0WriteBlock := Mux(i2Buf === 0.U, w1BufBlock(0), w1BufBlock(1))
    // start 同周期发起 col=0 的同步读，下一拍喂给 stream-in interp64。
    for (buf <- 0 until 2; sub <- 0 until 7) {
      when(i2Buf === buf.U) {
        w1Ram(buf)(sub).io.en := true.B
        w1Ram(buf)(sub).io.we := false.B
        w1Ram(buf)(sub).io.addr := 0.U(4.W)
      }
    }
    w1ReadCol := 1.U
    w1ReadFeedValid := true.B
    w1ReadFeedSel := 0.U
    w1ReadFeedCount := 0.U
    i2State := i2Run
  }.elsewhen(i2State === i2Run) {
    assert(w1ReadCol <= 64.U, "W1 stream read column must not exceed 64")
    when(w1ReadFeedValid) {
      assert(interp64Seq.io.inReady, "interp64Seq must be ready whenever W1 stream drives inValid")
      interp64Seq.io.inValid := true.B
      for (sub <- 0 until 7) {
        val word = Mux(i2Buf === 0.U, w1Ram(0)(sub).io.dout, w1Ram(1)(sub).io.dout)
        interp64Seq.io.inData(sub) := MuxLookup(w1ReadFeedSel, word(32, 0))(Seq(
          0.U -> word(32, 0),
          1.U -> word(65, 33),
          2.U -> word(98, 66),
          3.U -> word(131, 99)
        ))
      }
      w1ReadFeedCount := w1ReadFeedCount + 1.U
    }
    when(w1ReadCol === 64.U) {
      w1ReadFeedValid := false.B
    }.otherwise {
      for (buf <- 0 until 2; sub <- 0 until 7) {
        when(i2Buf === buf.U) {
          w1Ram(buf)(sub).io.en := true.B
          w1Ram(buf)(sub).io.we := false.B
          w1Ram(buf)(sub).io.addr := (w1ReadCol >> 2)(3, 0)
        }
      }
      w1ReadFeedSel := w1ReadCol(1, 0)
      w1ReadCol := w1ReadCol + 1.U
    }
    when(interp64Seq.io.outValid) {
      for (blk <- 0 until 7) {
        when(w0WriteBlock === blk.U) {
          w0Ram(blk).io.en := true.B
          w0Ram(blk).io.we := true.B
          // outBase = 4 * column，右移 2 位得到 64-depth SRAM 的列地址。
          w0Ram(blk).io.addr := (interp64Seq.io.outBase >> 2)(5, 0)
          w0Ram(blk).io.din := Cat(
            interp64Seq.io.outData(3), interp64Seq.io.outData(2),
            interp64Seq.io.outData(1), interp64Seq.io.outData(0)
          )
        }
      }
    }
    when(interp64Seq.io.done) {
      assert(w1ReadFeedCount === 64.U, "interp64 W1 stream should feed exactly 64 columns")
      w0Ready(w0WriteBlock) := true.B
      w1BlockReady(i2Buf) := false.B
      w1SubReady(i2Buf) := VecInit(Seq.fill(7)(false.B))
      w1BufValid(i2Buf) := false.B
      w1ReadFeedValid := false.B
      dbgInterp2Count := dbgInterp2Count + 1.U
      i2State := i2Idle
    }
  }

  when(i3State === i3Idle && busy && w0Ready.asUInt.andR) {
    i3State := i3Start
  }.elsewhen(i3State === i3Start) {
    interp256Seq.io.start := true.B
    // 启动 final 插值的同时发起 pairIdx=0 的同步读；下一拍即可喂入。
    for (g <- 0 until 7) {
      w0Ram(g).io.en := true.B
      w0Ram(g).io.we := false.B
      w0Ram(g).io.addr := 0.U(6.W)
    }
    w0ReadPairIdx := 1.U
    w0ReadFeedValid := true.B
    w0ReadFeedSel := false.B
    i3State := i3ReadPipe
  }.elsewhen(i3State === i3ReadPipe) {
    // 流水读：本拍消费上一拍 dout，同时发起下一 pair 读。
    // pairIdx even -> word(53,0)=Cat(c1,c0)，pairIdx odd -> word(107,54)=Cat(c3,c2)。
    when(w0ReadFeedValid) {
      interp256Seq.io.inValid := true.B
      for (g <- 0 until 7) {
        val d = w0Ram(g).io.dout
        interp256Seq.io.inPair(g) := Mux(w0ReadFeedSel, d(107, 54), d(53, 0))
      }
    }
    when(w0ReadPairIdx === 128.U) {
      w0ReadFeedValid := false.B
      i3State := i3Run
    }.otherwise {
      for (g <- 0 until 7) {
        w0Ram(g).io.en := true.B
        w0Ram(g).io.we := false.B
        w0Ram(g).io.addr := (w0ReadPairIdx >> 1)(5, 0)
      }
      w0ReadFeedSel := w0ReadPairIdx(0)
      w0ReadPairIdx := w0ReadPairIdx + 1.U
    }
  }.elsewhen(i3State === i3Run && interp256Seq.io.done) {
    // interp256Seq 内部已保存完整输出；同周期直接拉高 valid_out，一拍后回到 idle。
    io.valid_out := true.B
    busy := false.B
    i3State := i3Idle
  }

  // valid_in is accepted only when busy=false.
  // The caller/testbench must not assert valid_in while busy=true.
  // Future version may add ready_in for streaming multi-frame input.
  when(io.valid_in && !busy) {
    assert(!evalCoreQ.io.deq.valid, "evalCoreQ must be empty when accepting a new frame")
    regA := io.a
    regB := io.b
    busy := true.B
    pt0 := 0.U; pt1 := 0.U; pt2 := 0.U; evalPhase := 0.U; evalDone := false.B
    corePending := false.B
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
    w2WritePending := false.B; w2WriteGroup := 0.U; w2WriteBuf := 0.U; w2WritePt2 := 0.U; w2WritePt0 := 0.U; w2WritePt1 := 0.U
    w2ReadCol := 0.U; w2ReadFeedValid := false.B; w2ReadFeedSel := 0.U; w2ReadFeedCount := 0.U
    w1WriteBuf := 0.U; w1WriteSub := 0.U; w1ReadCol := 0.U; w1ReadFeedValid := false.B; w1ReadFeedSel := 0.U; w1ReadFeedCount := 0.U
    w0WriteBlock := 0.U; w0ReadPairIdx := 0.U; w0ReadFeedValid := false.B; w0ReadFeedSel := false.B
    w2WBuf := 0.U
    dbgCoreWriteCount := 0.U
    dbgInterp1Count := 0.U
    dbgInterp2Count := 0.U
  }

  io.dbg_core_write_count := dbgCoreWriteCount
  io.dbg_interp1_group_count := dbgInterp1Count
  io.dbg_interp2_block_count := dbgInterp2Count
}