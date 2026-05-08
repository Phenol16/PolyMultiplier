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
//  EvalBlock16TC4：一次性生成 Core16TC4 所需的 16 路三层 TC4 估值
//  - 不再使用 phase；每个 {pt0, pt1, pt2} 组合在组合逻辑中产生完整 16-lane job。
//  - 地址映射为 lane l -> in(l * 64 + offset)，与 EvalLaneFixed(l % 4, phase = l / 4) 等价。
// =============================================================================
class EvalBlock16TC4(memW: Int, outW: Int) extends Module {
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
        val base = l * 64 + 16 * k + 4 * j
        eval0.io.r(0) := io.in(base + 0)
        eval0.io.r(1) := io.in(base + 1)
        eval0.io.r(2) := io.in(base + 2)
        eval0.io.r(3) := io.in(base + 3)
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
    io.out(l)   := eval2.io.out
  }
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


class W2BlockDesc(bankW: Int) extends Bundle {
  val bank = UInt(bankW.W)
  val pt0  = UInt(3.W)
  val pt1  = UInt(3.W)
}

class W1BlockDesc(bankW: Int) extends Bundle {
  val bank = UInt(bankW.W)
  val pt0  = UInt(3.W)
}

class I1OutWord extends Bundle {
  val pt0  = UInt(3.W)
  val pt1  = UInt(3.W)
  val addr = UInt(4.W)
  val data = UInt(132.W)
}
// =============================================================================
//  ToomCook43 顶层
//  当前存储/调度结构：
//  - valid_in 时将输入 a/b 锁存到 regA/regB。
//  - EvalLaneFixed 生成 Core16 任务，Core16 输出写入 W2。
//  - W2 使用 SpRam(576, 2) + w2Local，保证 W2 写入一拍完成。
//  - interp16Seq 流式输出写 grouped W1 SRAM。
//  - interp64Seq 从 grouped W1 SRAM 按列流式输入，输出写 W0 SRAM。
//  - final interp256Seq 从 W0 SRAM 流式读取，内部保留最终输出缓存。
//  - valid_out 拉高一拍时输出 io.c。
// =============================================================================
class ToomCook43IO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(1024, UInt(24.W)))
  val b = Input(Vec(1024, UInt(8.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(1024, UInt(24.W)))
}

class ToomCook43Baseline extends Module {
  // ---------------------------------------------------------------------------
  // Local helpers
  // ---------------------------------------------------------------------------
  def packVec(xs: Seq[UInt]): UInt = Cat(xs.reverse)
  def unpackVec(x: UInt, n: Int, w: Int): Vec[UInt] = {
    val v = Wire(Vec(n, UInt(w.W)))
    for (i <- 0 until n) v(i) := x((i + 1) * w - 1, i * w)
    v
  }

  // ---------------------------------------------------------------------------
  // IO and constants
  // ---------------------------------------------------------------------------
  val io = IO(new ToomCook43IO)

  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W
  private val EVAL_LANES = 4

  // ---------------------------------------------------------------------------
  // Submodules
  // ---------------------------------------------------------------------------
  val evalLanesA = (0 until EVAL_LANES).map(l => Module(new EvalLaneFixed(24, A_EVAL_W, l, EVAL_LANES)))
  val evalLanesB = (0 until EVAL_LANES).map(l => Module(new EvalLaneFixed(8, B_EVAL_W, l, EVAL_LANES)))
  val evalCoreQ = Module(new Queue(new EvalCoreJob(A_EVAL_W, B_EVAL_W), 2, pipe = true, flow = false))
  val core = Module(new Core16TC4)
  val interp16Seq = Module(new InterpLayerSeqStreamTC4(16, 1, 36, 33))
  val interp64Seq = Module(new InterpLayerSeqStreamInOutTC4(64, 2, 33, 27))
  val interp256Seq = Module(new InterpLayerSeq2ColStreamInTC4(256, 3, 27, 24))

  // ---------------------------------------------------------------------------
  // Main storage resources
  // ---------------------------------------------------------------------------
  val regA = Reg(Vec(1024, UInt(24.W)))
  val regB = Reg(Vec(1024, UInt(8.W)))

  val w2Ram = Seq.fill(2, 7)(Module(new SpRam(576, 2)))
  val w2Local = Reg(Vec(7, Vec(16, UInt(36.W))))

  // W1 使用 grouped SRAM，每个 word 保存 4 个 33-bit 系数：Cat(c3,c2,c1,c0)。
  val w1Ram = Seq.fill(2, 7)(Module(new SpRam(132, 16)))

  // W0 使用 grouped SRAM，每个 word 保存 4 个 27-bit 系数：Cat(c3,c2,c1,c0)。
  val w0Ram = Seq.fill(7)(Module(new SpRam(108, 64)))

  // ---------------------------------------------------------------------------
  // Global frame control
  // ---------------------------------------------------------------------------
  val busy = RegInit(false.B)

  // ---------------------------------------------------------------------------
  // Eval/Core stage state
  // ---------------------------------------------------------------------------
  val evalPhase = RegInit(0.U(2.W))
  val pt0 = RegInit(0.U(3.W)); val pt1 = RegInit(0.U(3.W)); val pt2 = RegInit(0.U(3.W))
  val evalDone = RegInit(false.B)
  val avecBuild = Reg(Vec(16, UInt(A_EVAL_W.W)))
  val bvecBuild = Reg(Vec(16, UInt(B_EVAL_W.W)))

  val corePending = RegInit(false.B)
  val outPt0 = Reg(UInt(3.W)); val outPt1 = Reg(UInt(3.W)); val outPt2 = Reg(UInt(3.W))

  // ---------------------------------------------------------------------------
  // W2 buffer and I1 state
  // ---------------------------------------------------------------------------
  val w2WBuf = RegInit(0.U(1.W))
  val w2Empty = RegInit(VecInit(Seq.fill(2)(true.B)))
  val w2Writing = RegInit(VecInit(Seq.fill(2)(false.B)))
  val w2Reading = RegInit(VecInit(Seq.fill(2)(false.B)))
  val w2Ready = RegInit(VecInit(Seq.fill(2)(false.B)))
  val w2Full = RegInit(VecInit(Seq.fill(2) { VecInit(Seq.fill(7)(false.B)) }))
  val w2Pt0 = Reg(Vec(2, UInt(3.W))); val w2Pt1 = Reg(Vec(2, UInt(3.W)))

  val i1Idle :: i1ReadReq :: i1ReadCap :: i1Start :: i1Run :: Nil = Enum(5)
  val i1State = RegInit(i1Idle)
  val i1Buf = RegInit(0.U(1.W))
  val w1WriteBuf = RegInit(0.U(1.W))
  val w1WriteSub = RegInit(0.U(3.W))

  // ---------------------------------------------------------------------------
  // W1 buffer and I2 state
  // ---------------------------------------------------------------------------
  val w1BufValid = RegInit(VecInit(Seq.fill(2)(false.B)))
  val w1BufBlock = Reg(Vec(2, UInt(3.W)))
  val w1SubReady = RegInit(VecInit(Seq.fill(2) { VecInit(Seq.fill(7)(false.B)) }))
  val w1BlockReady = RegInit(VecInit(Seq.fill(2)(false.B)))

  val i2Idle :: i2Start :: i2Run :: Nil = Enum(3)
  val i2State = RegInit(i2Idle)
  val i2Buf = RegInit(0.U(1.W))
  val w1ReadCol = RegInit(0.U(7.W))
  val w1ReadFeedValid = RegInit(false.B)
  val w1ReadFeedSel = RegInit(0.U(2.W))
  val w0WriteBlock = RegInit(0.U(3.W))

  // ---------------------------------------------------------------------------
  // W0 readiness and I3 state
  // ---------------------------------------------------------------------------
  val w0Ready = RegInit(VecInit(Seq.fill(7)(false.B)))

  val i3Idle :: i3Start :: i3ReadPipe :: i3Run :: Nil = Enum(4)
  val i3State = RegInit(i3Idle)
  val w0ReadPairIdx = RegInit(0.U(8.W))
  val w0ReadFeedValid = RegInit(false.B)
  val w0ReadFeedSel = RegInit(false.B)

  // ---------------------------------------------------------------------------
  // Default outputs and child-module inputs
  // ---------------------------------------------------------------------------
  io.valid_out := false.B
  // final interp256Seq 内部保留输出缓存，以维持 io.c = Vec(1024, UInt(24.W)) 的并行输出接口。
  for (i <- 0 until 1024) io.c(i) := mask(interp256Seq.io.cOut(i), 24)

  interp16Seq.io.start := false.B
  for (i <- 0 until 7 * 16) interp16Seq.io.wIn(i) := w2Local(i / 16)(i % 16)

  interp64Seq.io.start := false.B
  interp64Seq.io.inValid := false.B
  for (s <- 0 until 7) interp64Seq.io.inData(s) := 0.U

  interp256Seq.io.start := false.B
  interp256Seq.io.inValid := false.B
  for (g <- 0 until 7) interp256Seq.io.inPair(g) := 0.U

  // ---------------------------------------------------------------------------
  // Default SRAM ports
  // ---------------------------------------------------------------------------
  for (b <- 0 until 2; p <- 0 until 7) {
    w2Ram(b)(p).io.clk := clock; w2Ram(b)(p).io.en := false.B; w2Ram(b)(p).io.we := false.B; w2Ram(b)(p).io.addr := 0.U(1.W); w2Ram(b)(p).io.din := 0.U
    w1Ram(b)(p).io.clk := clock; w1Ram(b)(p).io.en := false.B; w1Ram(b)(p).io.we := false.B; w1Ram(b)(p).io.addr := 0.U(4.W); w1Ram(b)(p).io.din := 0.U
  }
  for (g <- 0 until 7) {
    w0Ram(g).io.clk := clock; w0Ram(g).io.en := false.B; w0Ram(g).io.we := false.B; w0Ram(g).io.addr := 0.U(6.W); w0Ram(g).io.din := 0.U
  }

  // ---------------------------------------------------------------------------
  // Eval lane wiring
  // ---------------------------------------------------------------------------
  for (l <- 0 until EVAL_LANES) {
    evalLanesA(l).io.in := regA; evalLanesA(l).io.pt0 := pt0; evalLanesA(l).io.pt1 := pt1; evalLanesA(l).io.pt2 := pt2; evalLanesA(l).io.phase := evalPhase
    evalLanesB(l).io.in := regB; evalLanesB(l).io.pt0 := pt0; evalLanesB(l).io.pt1 := pt1; evalLanesB(l).io.pt2 := pt2; evalLanesB(l).io.phase := evalPhase
  }

  // ---------------------------------------------------------------------------
  // Eval accumulation and queue enqueue
  // ---------------------------------------------------------------------------
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

  // ---------------------------------------------------------------------------
  // Core dequeue and W2 write
  // ---------------------------------------------------------------------------
  core.io.valid_in := false.B
  core.io.avec := evalCoreQ.io.deq.bits.avec
  core.io.bvec := evalCoreQ.io.deq.bits.bvec
  val canUseW2WriteBuf = (w2Empty(w2WBuf) || w2Writing(w2WBuf)) && !w2Reading(w2WBuf) && !w2Ready(w2WBuf)
  val canCoreTake = busy && evalCoreQ.io.deq.valid && !corePending && canUseW2WriteBuf
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

  for (buf <- 0 until 2; p <- 0 until 7) {
    when(corePending && core.io.valid_out && w2WBuf === buf.U && outPt2 === p.U) {
      w2Ram(buf)(p).io.en := true.B
      w2Ram(buf)(p).io.we := true.B
      w2Ram(buf)(p).io.din := packVec(core.io.cOut)
    }
  }
  when(corePending && core.io.valid_out) {
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

  // ---------------------------------------------------------------------------
  // I1: W2 -> w2Local -> interp16 -> W1
  // ---------------------------------------------------------------------------
  when(i1State === i1Idle) {
    when(w2Ready(0) && !w2Writing(0)) { i1Buf := 0.U; w2Reading(0) := true.B; i1State := i1ReadReq }
      .elsewhen(w2Ready(1) && !w2Writing(1)) { i1Buf := 1.U; w2Reading(1) := true.B; i1State := i1ReadReq }
  }.elsewhen(i1State === i1ReadReq) {
    // i1ReadReq 发起同步 SRAM 读，i1ReadCap 捕获 dout 到 w2Local。
    for (buf <- 0 until 2) {
      when(i1Buf === buf.U) {
        for (p <- 0 until 7) { w2Ram(buf)(p).io.en := true.B; w2Ram(buf)(p).io.we := false.B }
      }
    }
    i1State := i1ReadCap
  }.elsewhen(i1State === i1ReadCap) {
    for (p <- 0 until 7) {
      val d = Mux(i1Buf === 0.U, w2Ram(0)(p).io.dout, w2Ram(1)(p).io.dout)
      w2Local(p) := unpackVec(d, 16, 36)
    }
    i1State := i1Start
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
      i1State := i1Run
    }
  }.elsewhen(i1State === i1Run) {
    when(interp16Seq.io.outValid) {
      for (buf <- 0 until 2; sub <- 0 until 7) {
        when(w1WriteBuf === buf.U && w1WriteSub === sub.U) {
          w1Ram(buf)(sub).io.en := true.B
          w1Ram(buf)(sub).io.we := true.B
          // W1 每个地址保存 interp16 输出的一整列 4 个 33-bit 系数。
          w1Ram(buf)(sub).io.addr := (interp16Seq.io.outBase >> 2)(3, 0)
          w1Ram(buf)(sub).io.din := Cat(
            interp16Seq.io.outData(3), interp16Seq.io.outData(2),
            interp16Seq.io.outData(1), interp16Seq.io.outData(0)
          )
        }
      }
    }
    when(interp16Seq.io.done) {
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
      i1State := i1Idle
    }
  }

  // ---------------------------------------------------------------------------
  // I2: W1 -> interp64 -> W0
  // ---------------------------------------------------------------------------
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
    i2State := i2Run
  }.elsewhen(i2State === i2Run) {
    when(w1ReadFeedValid) {
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
      w0Ready(w0WriteBlock) := true.B
      w1BlockReady(i2Buf) := false.B
      w1SubReady(i2Buf) := VecInit(Seq.fill(7)(false.B))
      w1BufValid(i2Buf) := false.B
      w1ReadFeedValid := false.B
      i2State := i2Idle
    }
  }

  // ---------------------------------------------------------------------------
  // I3: W0 -> interp256 -> output
  // ---------------------------------------------------------------------------
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

  // ---------------------------------------------------------------------------
  // New frame accept/reset
  // ---------------------------------------------------------------------------
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
    w1WriteBuf := 0.U; w1WriteSub := 0.U; w1ReadCol := 0.U; w1ReadFeedValid := false.B; w1ReadFeedSel := 0.U
    w0WriteBlock := 0.U; w0ReadPairIdx := 0.U; w0ReadFeedValid := false.B; w0ReadFeedSel := false.B
    w2WBuf := 0.U
  }
}
// =============================================================================
//  ToomCook43 新数据流顶层
//  EvalBlock16TC4 -> evalCoreQ -> Core16TC4 pipeline -> W2 bank pool
//  -> W2 descriptor queue -> 3 x I1 lanes -> I1 FIFOs -> W1 bank pool
//  -> W1 descriptor queue -> I2 -> W0 -> I3 -> output
// =============================================================================
class ToomCook43 extends Module {
  private def packVec(xs: Seq[UInt]): UInt = Cat(xs.reverse)

  val io = IO(new ToomCook43IO)

  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W
  private val W2_BANKS = 16
  private val W1_BANKS = 8
  private val I1_LANES = 3
  private val W2_BANK_W = log2Ceil(W2_BANKS)
  private val W1_BANK_W = log2Ceil(W1_BANKS)

  // Debug / latency counters are intentionally left as public Chisel vals so
  // local chiseltest specs can peek them without changing ToomCook43IO.
  val evalJobsIssued = RegInit(0.U(10.W))
  val coreJobsAccepted = RegInit(0.U(10.W))
  val coreJobsCompleted = RegInit(0.U(10.W))
  val w2BlocksCompleted = RegInit(0.U(6.W))
  val w1BlocksCompleted = RegInit(0.U(4.W))
  val w0BlocksCompleted = RegInit(0.U(4.W))

  val regA = Reg(Vec(1024, UInt(24.W)))
  val regB = Reg(Vec(1024, UInt(8.W)))
  val busy = RegInit(false.B)

  // ---------------------------------------------------------------------------
  // Eval stage: no phase/build registers; one enqueue opportunity per point triplet.
  // ---------------------------------------------------------------------------
  val pt0 = RegInit(0.U(3.W))
  val pt1 = RegInit(0.U(3.W))
  val pt2 = RegInit(0.U(3.W))
  val evalDone = RegInit(false.B)

  val evalAllA = Module(new EvalBlock16TC4(24, A_EVAL_W))
  val evalAllB = Module(new EvalBlock16TC4(8, B_EVAL_W))
  val evalCoreQ = Module(new Queue(new EvalCoreJob(A_EVAL_W, B_EVAL_W), 8, pipe = true, flow = false))

  evalAllA.io.in := regA
  evalAllA.io.pt0 := pt0
  evalAllA.io.pt1 := pt1
  evalAllA.io.pt2 := pt2
  evalAllB.io.in := regB
  evalAllB.io.pt0 := pt0
  evalAllB.io.pt1 := pt1
  evalAllB.io.pt2 := pt2

  val evalJobValid = busy && !evalDone
  evalCoreQ.io.enq.valid := evalJobValid
  evalCoreQ.io.enq.bits.avec := evalAllA.io.out
  evalCoreQ.io.enq.bits.bvec := evalAllB.io.out
  evalCoreQ.io.enq.bits.pt0 := pt0
  evalCoreQ.io.enq.bits.pt1 := pt1
  evalCoreQ.io.enq.bits.pt2 := pt2

  when(evalCoreQ.io.enq.fire) {
    evalJobsIssued := evalJobsIssued + 1.U
    when(pt0 === 6.U && pt1 === 6.U && pt2 === 6.U) {
      evalDone := true.B
    }.elsewhen(pt2 === 6.U) {
      pt2 := 0.U
      when(pt1 === 6.U) { pt1 := 0.U; pt0 := pt0 + 1.U }
        .otherwise { pt1 := pt1 + 1.U }
    }.otherwise {
      pt2 := pt2 + 1.U
    }
  }

  // ---------------------------------------------------------------------------
  // W2 bank pool and Core pipeline metadata.
  // ---------------------------------------------------------------------------
  val core = Module(new Core16TC4)
  val w2ReadyQ = Module(new Queue(new W2BlockDesc(W2_BANK_W), W2_BANKS, pipe = true, flow = false))
  val w2Data = Reg(Vec(W2_BANKS, Vec(7, Vec(16, UInt(36.W)))))
  val w2Valid = RegInit(VecInit(Seq.fill(W2_BANKS)(false.B)))
  val w2Consuming = RegInit(VecInit(Seq.fill(W2_BANKS)(false.B)))
  val w2Pt0 = Reg(Vec(W2_BANKS, UInt(3.W)))
  val w2Pt1 = Reg(Vec(W2_BANKS, UInt(3.W)))
  val w2SubReady = RegInit(VecInit(Seq.fill(W2_BANKS) { VecInit(Seq.fill(7)(false.B)) }))

  val w2HitVec = Wire(Vec(W2_BANKS, Bool()))
  val w2FreeVec = Wire(Vec(W2_BANKS, Bool()))
  for (b <- 0 until W2_BANKS) {
    w2HitVec(b) := w2Valid(b) && !w2Consuming(b) && w2Pt0(b) === evalCoreQ.io.deq.bits.pt0 && w2Pt1(b) === evalCoreQ.io.deq.bits.pt1
    w2FreeVec(b) := !w2Valid(b) && !w2Consuming(b)
  }
  val w2HasHit = w2HitVec.asUInt.orR
  val w2HasFree = w2FreeVec.asUInt.orR
  val w2HitOH = PriorityEncoderOH(w2HitVec)
  val w2FreeOH = PriorityEncoderOH(w2FreeVec)
  val allocatedW2Bank = OHToUInt(Mux(w2HasHit, w2HitOH, w2FreeOH))
  val w2NeedsReadyDesc = evalCoreQ.io.deq.bits.pt2 === 6.U
  val w2CanAcceptThisJob = evalCoreQ.io.deq.valid && (w2HasHit || w2HasFree) && (!w2NeedsReadyDesc || w2ReadyQ.io.enq.ready)
  val canCoreTake = busy && w2CanAcceptThisJob

  core.io.valid_in := canCoreTake
  core.io.avec := evalCoreQ.io.deq.bits.avec
  core.io.bvec := evalCoreQ.io.deq.bits.bvec
  evalCoreQ.io.deq.ready := canCoreTake

  val coreMetaValid = RegNext(canCoreTake, false.B)
  val coreMetaPt0 = RegEnable(evalCoreQ.io.deq.bits.pt0, canCoreTake)
  val coreMetaPt1 = RegEnable(evalCoreQ.io.deq.bits.pt1, canCoreTake)
  val coreMetaPt2 = RegEnable(evalCoreQ.io.deq.bits.pt2, canCoreTake)
  val coreMetaBank = RegEnable(allocatedW2Bank, canCoreTake)

  when(canCoreTake) {
    coreJobsAccepted := coreJobsAccepted + 1.U
    w2Valid(allocatedW2Bank) := true.B
    w2Pt0(allocatedW2Bank) := evalCoreQ.io.deq.bits.pt0
    w2Pt1(allocatedW2Bank) := evalCoreQ.io.deq.bits.pt1
    when(!w2HasHit) {
      w2SubReady(allocatedW2Bank) := VecInit(Seq.fill(7)(false.B))
    }
  }

  val w2Completes = WireDefault(false.B)
  val w2CompleteBank = WireDefault(0.U(W2_BANK_W.W))
  val w2CompletePt0 = WireDefault(0.U(3.W))
  val w2CompletePt1 = WireDefault(0.U(3.W))

  when(core.io.valid_out && coreMetaValid) {
    coreJobsCompleted := coreJobsCompleted + 1.U
    for (i <- 0 until 16) {
      w2Data(coreMetaBank)(coreMetaPt2)(i) := core.io.cOut(i)
    }
    val nextReady = Wire(Vec(7, Bool()))
    nextReady := w2SubReady(coreMetaBank)
    nextReady(coreMetaPt2) := true.B
    w2SubReady(coreMetaBank) := nextReady
    when(nextReady.asUInt.andR) {
      w2Completes := true.B
      w2CompleteBank := coreMetaBank
      w2CompletePt0 := coreMetaPt0
      w2CompletePt1 := coreMetaPt1
      w2BlocksCompleted := w2BlocksCompleted + 1.U
    }
  }

  w2ReadyQ.io.enq.valid := w2Completes
  w2ReadyQ.io.enq.bits.bank := w2CompleteBank
  w2ReadyQ.io.enq.bits.pt0 := w2CompletePt0
  w2ReadyQ.io.enq.bits.pt1 := w2CompletePt1

  // ---------------------------------------------------------------------------
  // W1 bank pool: one bank per pt0 block, seven pt1 sub-blocks per bank.
  // ---------------------------------------------------------------------------
  val w1ReadyQ = Module(new Queue(new W1BlockDesc(W1_BANK_W), W1_BANKS, pipe = true, flow = false))
  val w1Data = Reg(Vec(W1_BANKS, Vec(7, Vec(16, UInt(132.W)))))
  val w1Valid = RegInit(VecInit(Seq.fill(W1_BANKS)(false.B)))
  val w1Reading = RegInit(VecInit(Seq.fill(W1_BANKS)(false.B)))
  val w1Pt0 = Reg(Vec(W1_BANKS, UInt(3.W)))
  val w1SubReady = RegInit(VecInit(Seq.fill(W1_BANKS) { VecInit(Seq.fill(7)(false.B)) }))

  def w1CanHold(pt: UInt): Bool = {
    val hit = Wire(Vec(W1_BANKS, Bool()))
    val free = Wire(Vec(W1_BANKS, Bool()))
    for (b <- 0 until W1_BANKS) {
      hit(b) := w1Valid(b) && !w1Reading(b) && w1Pt0(b) === pt
      free(b) := !w1Valid(b) && !w1Reading(b)
    }
    hit.asUInt.orR || free.asUInt.orR
  }

  def w1SelectBank(pt: UInt): UInt = {
    val hit = Wire(Vec(W1_BANKS, Bool()))
    val free = Wire(Vec(W1_BANKS, Bool()))
    for (b <- 0 until W1_BANKS) {
      hit(b) := w1Valid(b) && !w1Reading(b) && w1Pt0(b) === pt
      free(b) := !w1Valid(b) && !w1Reading(b)
    }
    OHToUInt(Mux(hit.asUInt.orR, PriorityEncoderOH(hit), PriorityEncoderOH(free)))
  }

  // ---------------------------------------------------------------------------
  // I1 lanes and output FIFOs.
  // ---------------------------------------------------------------------------
  val interp16 = Seq.fill(I1_LANES)(Module(new InterpLayerSeqStreamTC4(16, 1, 36, 33)))
  val i1OutQs = Seq.fill(I1_LANES)(Module(new Queue(new I1OutWord, 32, pipe = true, flow = false)))
  val i1Idle :: i1Run :: Nil = Enum(2)
  val i1State = RegInit(VecInit(Seq.fill(I1_LANES)(i1Idle)))
  val i1Bank = Reg(Vec(I1_LANES, UInt(W2_BANK_W.W)))
  val i1Pt0 = Reg(Vec(I1_LANES, UInt(3.W)))
  val i1Pt1 = Reg(Vec(I1_LANES, UInt(3.W)))

  for (l <- 0 until I1_LANES) {
    interp16(l).io.start := false.B
    for (p <- 0 until 7; i <- 0 until 16) {
      interp16(l).io.wIn(p * 16 + i) := w2Data(i1Bank(l))(p)(i)
    }

    i1OutQs(l).io.enq.valid := interp16(l).io.outValid
    i1OutQs(l).io.enq.bits.pt0 := i1Pt0(l)
    i1OutQs(l).io.enq.bits.pt1 := i1Pt1(l)
    i1OutQs(l).io.enq.bits.addr := (interp16(l).io.outBase >> 2)(3, 0)
    i1OutQs(l).io.enq.bits.data := Cat(interp16(l).io.outData(3), interp16(l).io.outData(2), interp16(l).io.outData(1), interp16(l).io.outData(0))
    assert(!interp16(l).io.outValid || i1OutQs(l).io.enq.ready, "I1 output FIFO overflow; increase depth or add decoupled I1 backpressure")

    when(i1State(l) === i1Run && interp16(l).io.done) {
      w2Valid(i1Bank(l)) := false.B
      w2Consuming(i1Bank(l)) := false.B
      w2SubReady(i1Bank(l)) := VecInit(Seq.fill(7)(false.B))
      i1State(l) := i1Idle
    }
  }

  val idleLaneVec = Wire(Vec(I1_LANES, Bool()))
  for (l <- 0 until I1_LANES) idleLaneVec(l) := i1State(l) === i1Idle
  val hasIdleLane = idleLaneVec.asUInt.orR
  val takeI1Desc = busy && w2ReadyQ.io.deq.valid && hasIdleLane && w1CanHold(w2ReadyQ.io.deq.bits.pt0)
  val i1SelLane = OHToUInt(PriorityEncoderOH(idleLaneVec))
  w2ReadyQ.io.deq.ready := takeI1Desc

  when(takeI1Desc) {
    val w1BankForLane = w1SelectBank(w2ReadyQ.io.deq.bits.pt0)
    i1State(i1SelLane) := i1Run
    i1Bank(i1SelLane) := w2ReadyQ.io.deq.bits.bank
    i1Pt0(i1SelLane) := w2ReadyQ.io.deq.bits.pt0
    i1Pt1(i1SelLane) := w2ReadyQ.io.deq.bits.pt1
    w2Consuming(w2ReadyQ.io.deq.bits.bank) := true.B
    when(!w1Valid(w1BankForLane)) {
      w1Valid(w1BankForLane) := true.B
      w1Pt0(w1BankForLane) := w2ReadyQ.io.deq.bits.pt0
      w1SubReady(w1BankForLane) := VecInit(Seq.fill(7)(false.B))
    }
    for (l <- 0 until I1_LANES) {
      when(i1SelLane === l.U) { interp16(l).io.start := true.B }
    }
  }

  // ---------------------------------------------------------------------------
  // W1 writer: one unified writer arbitrates I1 lane FIFOs and emits W1 descriptors.
  // ---------------------------------------------------------------------------
  for (l <- 0 until I1_LANES) i1OutQs(l).io.deq.ready := false.B

  val w1WriteValidVec = Wire(Vec(I1_LANES, Bool()))
  for (l <- 0 until I1_LANES) w1WriteValidVec(l) := i1OutQs(l).io.deq.valid
  val w1WriterHasWord = w1WriteValidVec.asUInt.orR
  val w1WriterSelOH = PriorityEncoderOH(w1WriteValidVec)
  val w1WriterSel = OHToUInt(w1WriterSelOH)
  val w1Word = Wire(new I1OutWord)
  w1Word := Mux1H(w1WriterSelOH, i1OutQs.map(_.io.deq.bits))

  val w1WriterBank = w1SelectBank(w1Word.pt0)
  val w1CompletingSub = w1Word.addr === 0.U
  val w1NextSubReady = Wire(Vec(7, Bool()))
  w1NextSubReady := w1SubReady(w1WriterBank)
  w1NextSubReady(w1Word.pt1) := true.B
  val w1CompletingBlock = w1CompletingSub && w1NextSubReady.asUInt.andR
  val w1WriterCanCommit = w1WriterHasWord && w1CanHold(w1Word.pt0) && (!w1CompletingBlock || w1ReadyQ.io.enq.ready)

  w1ReadyQ.io.enq.valid := w1WriterCanCommit && w1CompletingBlock
  w1ReadyQ.io.enq.bits.bank := w1WriterBank
  w1ReadyQ.io.enq.bits.pt0 := w1Word.pt0

  when(w1WriterCanCommit) {
    for (l <- 0 until I1_LANES) {
      when(w1WriterSel === l.U) { i1OutQs(l).io.deq.ready := true.B }
    }
    w1Data(w1WriterBank)(w1Word.pt1)(w1Word.addr) := w1Word.data
    when(w1CompletingSub) {
      w1SubReady(w1WriterBank) := w1NextSubReady
      when(w1NextSubReady.asUInt.andR) {
        w1BlocksCompleted := w1BlocksCompleted + 1.U
      }
    }
  }

  // ---------------------------------------------------------------------------
  // I2: descriptorized W1 -> interp64 -> W0 register bank.
  // ---------------------------------------------------------------------------
  val interp64Seq = Module(new InterpLayerSeqStreamInOutTC4(64, 2, 33, 27))
  val w0Data = Reg(Vec(7, Vec(64, UInt(108.W))))
  val w0Ready = RegInit(VecInit(Seq.fill(7)(false.B)))
  val i2Idle :: i2Start :: i2Run :: Nil = Enum(3)
  val i2State = RegInit(i2Idle)
  val i2Bank = Reg(UInt(W1_BANK_W.W))
  val i2Pt0 = Reg(UInt(3.W))
  val i2Col = RegInit(0.U(7.W))

  interp64Seq.io.start := false.B
  interp64Seq.io.inValid := false.B
  for (p <- 0 until 7) interp64Seq.io.inData(p) := 0.U
  w1ReadyQ.io.deq.ready := false.B

  when(i2State === i2Idle) {
    w1ReadyQ.io.deq.ready := busy && w1ReadyQ.io.deq.valid
    when(w1ReadyQ.io.deq.fire) {
      i2Bank := w1ReadyQ.io.deq.bits.bank
      i2Pt0 := w1ReadyQ.io.deq.bits.pt0
      w1Reading(w1ReadyQ.io.deq.bits.bank) := true.B
      i2State := i2Start
    }
  }.elsewhen(i2State === i2Start) {
    interp64Seq.io.start := true.B
    i2Col := 0.U
    i2State := i2Run
  }.elsewhen(i2State === i2Run) {
    when(i2Col < 64.U && interp64Seq.io.inReady) {
      interp64Seq.io.inValid := true.B
      for (sub <- 0 until 7) {
        val word = w1Data(i2Bank)(sub)((i2Col >> 2)(3, 0))
        interp64Seq.io.inData(sub) := MuxLookup(i2Col(1, 0), word(32, 0))(Seq(
          0.U -> word(32, 0),
          1.U -> word(65, 33),
          2.U -> word(98, 66),
          3.U -> word(131, 99)
        ))
      }
      i2Col := i2Col + 1.U
    }
    when(interp64Seq.io.outValid) {
      w0Data(i2Pt0)((interp64Seq.io.outBase >> 2)(5, 0)) := Cat(interp64Seq.io.outData(3), interp64Seq.io.outData(2), interp64Seq.io.outData(1), interp64Seq.io.outData(0))
    }
    when(interp64Seq.io.done) {
      w0Ready(i2Pt0) := true.B
      w0BlocksCompleted := w0BlocksCompleted + 1.U
      w1Valid(i2Bank) := false.B
      w1Reading(i2Bank) := false.B
      w1SubReady(i2Bank) := VecInit(Seq.fill(7)(false.B))
      i2State := i2Idle
    }
  }

  // ---------------------------------------------------------------------------
  // I3: final interpolation from W0 once all seven pt0 blocks are ready.
  // ---------------------------------------------------------------------------
  val interp256Seq = Module(new InterpLayerSeq2ColStreamInTC4(256, 3, 27, 24))
  val i3Idle :: i3Start :: i3Run :: Nil = Enum(3)
  val i3State = RegInit(i3Idle)
  val i3Pair = RegInit(0.U(8.W))

  interp256Seq.io.start := false.B
  interp256Seq.io.inValid := false.B
  for (g <- 0 until 7) interp256Seq.io.inPair(g) := 0.U

  io.valid_out := false.B
  for (i <- 0 until 1024) io.c(i) := mask(interp256Seq.io.cOut(i), 24)

  when(i3State === i3Idle && busy && w0Ready.asUInt.andR) {
    i3State := i3Start
  }.elsewhen(i3State === i3Start) {
    interp256Seq.io.start := true.B
    i3Pair := 0.U
    i3State := i3Run
  }.elsewhen(i3State === i3Run) {
    when(i3Pair < 128.U && interp256Seq.io.inReady) {
      interp256Seq.io.inValid := true.B
      for (g <- 0 until 7) {
        val word = w0Data(g)((i3Pair >> 1)(5, 0))
        interp256Seq.io.inPair(g) := Mux(i3Pair(0), word(107, 54), word(53, 0))
      }
      i3Pair := i3Pair + 1.U
    }
    when(interp256Seq.io.done) {
      io.valid_out := true.B
      busy := false.B
      i3State := i3Idle
    }
  }

  // ---------------------------------------------------------------------------
  // Frame reset / accept. Top-level IO remains unchanged; one frame in flight.
  // ---------------------------------------------------------------------------
  when(io.valid_in && !busy) {
    regA := io.a
    regB := io.b
    busy := true.B
    pt0 := 0.U; pt1 := 0.U; pt2 := 0.U; evalDone := false.B
    evalJobsIssued := 0.U; coreJobsAccepted := 0.U; coreJobsCompleted := 0.U
    w2BlocksCompleted := 0.U; w1BlocksCompleted := 0.U; w0BlocksCompleted := 0.U
    w2Valid := VecInit(Seq.fill(W2_BANKS)(false.B))
    w2Consuming := VecInit(Seq.fill(W2_BANKS)(false.B))
    w2SubReady := VecInit(Seq.fill(W2_BANKS) { VecInit(Seq.fill(7)(false.B)) })
    w1Valid := VecInit(Seq.fill(W1_BANKS)(false.B))
    w1Reading := VecInit(Seq.fill(W1_BANKS)(false.B))
    w1SubReady := VecInit(Seq.fill(W1_BANKS) { VecInit(Seq.fill(7)(false.B)) })
    w0Ready := VecInit(Seq.fill(7)(false.B))
    for (l <- 0 until I1_LANES) i1State(l) := i1Idle
    i2State := i2Idle
    i3State := i3Idle
  }
}
