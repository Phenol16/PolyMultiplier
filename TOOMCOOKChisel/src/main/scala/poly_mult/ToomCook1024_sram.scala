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
    val phase = Input(UInt(math.max(1, log2Ceil(evalPhases)).W))
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
//  SRAM-connected modular Toom-Cook top-level data types
// =============================================================================
class EvalJobTC4 extends Bundle {
  val pt0  = UInt(3.W)
  val pt1  = UInt(3.W)
  val pt2  = UInt(3.W)
  val avec = Vec(16, UInt(TC4EvalWidth.A_EVAL_W.W))
  val bvec = Vec(16, UInt(TC4EvalWidth.B_EVAL_W.W))
}

class CoreResultTC4 extends Bundle {
  val pt0  = UInt(3.W)
  val pt1  = UInt(3.W)
  val pt2  = UInt(3.W)
  val cOut = Vec(16, UInt(36.W))
}

/** SRAM-like frame buffer for EvalModule -> CoreModule traffic.
  *
  * Address mapping is intentionally visible at this boundary:
  *   page = pt0 * 7 + pt1  (49 pages)
  *   bank = pt2            (7 banks per page)
  * Each (page, bank) entry stores exactly one EvalJob.
  */
class EvalCoreSRAM extends Module {
  val io = IO(new Bundle {
    val we   = Input(Bool())
    val re   = Input(Bool())
    val page = Input(UInt(6.W))
    val bank = Input(UInt(3.W))
    val din  = Input(new EvalJobTC4)
    val dout = Output(new EvalJobTC4)
  })

  val mem = Reg(Vec(49, Vec(7, new EvalJobTC4)))
  when(io.we) { mem(io.page)(io.bank) := io.din }
  io.dout := mem(io.page)(io.bank)
}

/** SRAM-like frame buffer for CoreModule -> InterModule traffic.
  *
  * The page/bank mapping is identical to EvalCoreSRAM:
  *   page = pt0 * 7 + pt1
  *   bank = pt2
  * Each entry stores the local Core16TC4 result Vec(16, UInt(36.W)); packed
  * implementations may treat this as one 16 * 36 = 576-bit word.
  */
class CoreInterSRAM extends Module {
  val io = IO(new Bundle {
    val we   = Input(Bool())
    val re   = Input(Bool())
    val page = Input(UInt(6.W))
    val bank = Input(UInt(3.W))
    val din  = Input(Vec(16, UInt(36.W)))
    val dout = Output(Vec(16, UInt(36.W)))
  })

  val mem = Reg(Vec(49, Vec(7, Vec(16, UInt(36.W)))))
  when(io.we) { mem(io.page)(io.bank) := io.din }
  io.dout := mem(io.page)(io.bank)
}

/** Four-column interpolation helper.
  *
  * Input layout is point-major: in(pt * 4 + col). Output layout is column-major:
  * out(col * 4 + coeff). The core chain and final column-zero correction mirror
  * InterpLayerTC4, with valid_out registered one cycle after valid_in.
  */
class Interp4ColsTC4(pidx: Int, inW: Int, outW: Int) extends Module {
  private val p = InterpParamTable.params(pidx)
  private val mk2 = p.mk2

  val io = IO(new Bundle {
    val valid_in  = Input(Bool())
    val in        = Input(Vec(28, UInt(inW.W)))
    val valid_out = Output(Bool())
    val out       = Output(Vec(16, UInt(outW.W)))
  })

  val cRaw  = Wire(Vec(16, UInt(outW.W)))
  val prevR0 = Wire(Vec(5, UInt(mk2.W)))
  val prevR1 = Wire(Vec(5, UInt(mk2.W)))
  val prevR2 = Wire(Vec(5, UInt(mk2.W)))
  prevR0(0) := 0.U; prevR1(0) := 0.U; prevR2(0) := 0.U

  for (col <- 0 until 4) {
    val core = Module(new InterpCoreTC4(pidx, inW))
    for (pt <- 0 until 7) core.io.pIn(pt) := io.in(pt * 4 + col)
    core.io.pr0 := prevR0(col)
    core.io.pr1 := prevR1(col)
    core.io.pr2 := prevR2(col)
    cRaw(4 * col + 0) := mask(core.io.c0part, outW)
    cRaw(4 * col + 1) := mask(core.io.c1part, outW)
    cRaw(4 * col + 2) := mask(core.io.c2part, outW)
    cRaw(4 * col + 3) := mask(core.io.c3, outW)
    prevR0(col + 1) := core.io.nr0
    prevR1(col + 1) := core.io.nr1
    prevR2(col + 1) := core.io.nr2
  }

  val fixed = Wire(Vec(16, UInt(outW.W)))
  fixed := cRaw
  fixed(0) := mask(cRaw(0) - prevR2(4), outW)
  fixed(1) := mask(cRaw(1) - prevR1(4), outW)
  fixed(2) := mask(cRaw(2) - prevR0(4), outW)

  io.valid_out := RegNext(io.valid_in, false.B)
  io.out := RegEnable(fixed, io.valid_in)
}

class EvalModule extends Module {
  private val EVAL_LANES = 16
  val io = IO(new Bundle {
    val start = Input(Bool())
    val a = Input(Vec(1024, UInt(24.W)))
    val b = Input(Vec(1024, UInt(8.W)))
    val sramWe = Output(Bool())
    val sramPage = Output(UInt(6.W))
    val sramBank = Output(UInt(3.W))
    val sramDin = Output(new EvalJobTC4)
    val done = Output(Bool())
    val debugEvalJobCount = Output(UInt(10.W))
  })

  val running = RegInit(false.B)
  val pt0 = RegInit(0.U(3.W)); val pt1 = RegInit(0.U(3.W)); val pt2 = RegInit(0.U(3.W))
  val jobCount = RegInit(0.U(10.W))
  val doneReg = RegInit(false.B)

  val evalA = (0 until EVAL_LANES).map(l => Module(new EvalLaneFixed(24, TC4EvalWidth.A_EVAL_W, l, EVAL_LANES)))
  val evalB = (0 until EVAL_LANES).map(l => Module(new EvalLaneFixed(8, TC4EvalWidth.B_EVAL_W, l, EVAL_LANES)))
  for (l <- 0 until EVAL_LANES) {
    evalA(l).io.in := io.a; evalA(l).io.pt0 := pt0; evalA(l).io.pt1 := pt1; evalA(l).io.pt2 := pt2; evalA(l).io.phase := 0.U
    evalB(l).io.in := io.b; evalB(l).io.pt0 := pt0; evalB(l).io.pt1 := pt1; evalB(l).io.pt2 := pt2; evalB(l).io.phase := 0.U
  }

  io.sramWe := running
  io.sramPage := pt0 * 7.U + pt1
  io.sramBank := pt2
  io.sramDin.pt0 := pt0; io.sramDin.pt1 := pt1; io.sramDin.pt2 := pt2
  for (l <- 0 until EVAL_LANES) { io.sramDin.avec(l) := evalA(l).io.out; io.sramDin.bvec(l) := evalB(l).io.out }
  io.done := doneReg
  io.debugEvalJobCount := jobCount

  when(doneReg) { doneReg := false.B }
  when(io.start && !running) {
    running := true.B; pt0 := 0.U; pt1 := 0.U; pt2 := 0.U; jobCount := 0.U; doneReg := false.B
  }.elsewhen(running) {
    jobCount := jobCount + 1.U
    when(pt0 === 6.U && pt1 === 6.U && pt2 === 6.U) {
      running := false.B; doneReg := true.B
      assert(jobCount + 1.U === 343.U, "EvalModule must complete exactly 343 EvalJobs")
    }.otherwise {
      when(pt2 === 6.U) { pt2 := 0.U; when(pt1 === 6.U) { pt1 := 0.U; pt0 := pt0 + 1.U }.otherwise { pt1 := pt1 + 1.U } }
        .otherwise { pt2 := pt2 + 1.U }
    }
  }
}

class CoreModule extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val evalRe = Output(Bool())
    val evalPage = Output(UInt(6.W))
    val evalBank = Output(UInt(3.W))
    val evalDout = Input(new EvalJobTC4)
    val coreWe = Output(Bool())
    val corePage = Output(UInt(6.W))
    val coreBank = Output(UInt(3.W))
    val coreDin = Output(Vec(16, UInt(36.W)))
    val done = Output(Bool())
    val debugCoreJobCount = Output(UInt(10.W))
  })

  val core = Module(new Core16TC4)
  val running = RegInit(false.B)
  val issueCount = RegInit(0.U(10.W))
  val writeCount = RegInit(0.U(10.W))
  val metaPage = RegInit(0.U(6.W)); val metaBank = RegInit(0.U(3.W))
  val doneReg = RegInit(false.B)

  val issueValid = running && issueCount < 343.U
  val page = (issueCount / 7.U)(5, 0)
  val bank = (issueCount % 7.U)(2, 0)

  io.evalRe := issueValid
  io.evalPage := page
  io.evalBank := bank
  core.io.valid_in := issueValid
  core.io.avec := io.evalDout.avec
  core.io.bvec := io.evalDout.bvec

  io.coreWe := core.io.valid_out
  io.corePage := metaPage
  io.coreBank := metaBank
  io.coreDin := core.io.cOut
  io.done := doneReg
  io.debugCoreJobCount := writeCount

  when(doneReg) { doneReg := false.B }
  when(io.start && !running) {
    running := true.B; issueCount := 0.U; writeCount := 0.U; doneReg := false.B
  }.elsewhen(running) {
    when(issueValid) { issueCount := issueCount + 1.U; metaPage := page; metaBank := bank }
    when(core.io.valid_out) {
      writeCount := writeCount + 1.U
      when(writeCount + 1.U === 343.U) {
        running := false.B; doneReg := true.B
        assert(issueCount === 343.U, "CoreModule cannot finish before all 343 jobs are issued")
      }
    }
  }
}

class InterModule extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val coreRe = Output(Bool())
    val corePage = Output(UInt(6.W))
    val coreBank = Output(UInt(3.W))
    val coreDout = Input(Vec(16, UInt(36.W)))
    val done = Output(Bool())
    val c = Output(Vec(1024, UInt(24.W)))
    val debugInterLayer1GroupCount = Output(UInt(6.W))
    val debugInterLayer2GroupCount = Output(UInt(3.W))
    val debugInterLayer3GroupCount = Output(UInt(1.W))
  })

  val regW2Page = Reg(Vec(7 * 16, UInt(36.W)))
  val regW1 = Reg(Vec(7, Vec(7, Vec(64, UInt(33.W)))))
  val regW0 = Reg(Vec(7, Vec(256, UInt(27.W))))
  val regC = Reg(Vec(1024, UInt(24.W)))

  val interp16 = Module(new InterpLayerTC4(16, 1, 36, 33))
  val interp64 = Module(new InterpLayerTC4(64, 2, 33, 27))
  val interp256 = Module(new InterpLayerTC4(256, 3, 27, 24))
  for (i <- 0 until 112) interp16.io.wIn(i) := regW2Page(i)

  val blockCnt = RegInit(0.U(3.W))
  val subCnt = RegInit(0.U(3.W))
  val pageCnt = RegInit(0.U(6.W))
  val bankCnt = RegInit(0.U(3.W))
  val l1Count = RegInit(0.U(6.W))
  val l2Count = RegInit(0.U(3.W))
  val l3Count = RegInit(0.U(1.W))
  val doneReg = RegInit(false.B)

  val sIdle :: sL1Read :: sL1Store :: sL2Store :: sL3Store :: Nil = Enum(5)
  val state = RegInit(sIdle)

  io.coreRe := state === sL1Read
  io.corePage := pageCnt
  io.coreBank := bankCnt
  io.done := doneReg
  io.c := regC
  io.debugInterLayer1GroupCount := l1Count
  io.debugInterLayer2GroupCount := l2Count
  io.debugInterLayer3GroupCount := l3Count

  for (sub <- 0 until 7; k <- 0 until 64) interp64.io.wIn(sub * 64 + k) := regW1(blockCnt)(sub)(k)
  for (g <- 0 until 7; k <- 0 until 256) interp256.io.wIn(g * 256 + k) := regW0(g)(k)

  when(doneReg) { doneReg := false.B }
  switch(state) {
    is(sIdle) {
      when(io.start) {
        blockCnt := 0.U; subCnt := 0.U; pageCnt := 0.U; bankCnt := 0.U
        l1Count := 0.U; l2Count := 0.U; l3Count := 0.U; doneReg := false.B
        state := sL1Read
      }
    }
    is(sL1Read) {
      for (t <- 0 until 16) regW2Page(bankCnt * 16.U + t.U) := io.coreDout(t)
      when(bankCnt === 6.U) { bankCnt := 0.U; state := sL1Store }.otherwise { bankCnt := bankCnt + 1.U }
    }
    is(sL1Store) {
      for (k <- 0 until 64) regW1(blockCnt)(subCnt)(k) := interp16.io.cOut(k)
      l1Count := l1Count + 1.U
      when(pageCnt === 48.U) { blockCnt := 0.U; state := sL2Store }
      .otherwise {
        pageCnt := pageCnt + 1.U
        when(subCnt === 6.U) { subCnt := 0.U; blockCnt := blockCnt + 1.U }.otherwise { subCnt := subCnt + 1.U }
        state := sL1Read
      }
    }
    is(sL2Store) {
      for (k <- 0 until 256) regW0(blockCnt)(k) := interp64.io.cOut(k)
      l2Count := l2Count + 1.U
      when(blockCnt === 6.U) { blockCnt := 0.U; state := sL3Store }
        .otherwise { blockCnt := blockCnt + 1.U }
    }
    is(sL3Store) {
      for (i <- 0 until 1024) regC(i) := mask(interp256.io.cOut(i), 24)
      l3Count := 1.U
      doneReg := true.B
      assert(l1Count === 49.U, "InterModule layer 1 must consume 49 groups")
      assert(l2Count === 7.U, "InterModule layer 2 must consume 7 groups")
      state := sIdle
    }
  }
}

class ToomCook43IO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(1024, UInt(24.W)))
  val b = Input(Vec(1024, UInt(8.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(1024, UInt(24.W)))
}

/** Clarity-first SRAM-connected Toom-Cook-4^3 top.
  *
  * The external Vec input is copied into an InputFrameRegCache during sLoadInput.
  * This intentionally simple cache stands in for a true banked Input SRAM and can
  * be replaced later without changing EvalModule/CoreModule/InterModule.
  */
class ToomCook43 extends Module {
  val io = IO(new ToomCook43IO)

  val inputA = Reg(Vec(1024, UInt(24.W)))
  val inputB = Reg(Vec(1024, UInt(8.W)))

  val evalModule = Module(new EvalModule)
  val coreModule = Module(new CoreModule)
  val interModule = Module(new InterModule)
  val evalCoreSram = Module(new EvalCoreSRAM)
  val coreInterSram = Module(new CoreInterSRAM)

  val sIdle :: sLoadInput :: sEval :: sCore :: sInter :: sDone :: Nil = Enum(6)
  val state = RegInit(sIdle)

  io.valid_out := state === sDone
  io.c := interModule.io.c

  evalModule.io.start := false.B
  evalModule.io.a := inputA
  evalModule.io.b := inputB
  coreModule.io.start := false.B
  interModule.io.start := false.B

  evalCoreSram.io.we := evalModule.io.sramWe
  evalCoreSram.io.re := coreModule.io.evalRe
  evalCoreSram.io.page := Mux(state === sCore, coreModule.io.evalPage, evalModule.io.sramPage)
  evalCoreSram.io.bank := Mux(state === sCore, coreModule.io.evalBank, evalModule.io.sramBank)
  evalCoreSram.io.din := evalModule.io.sramDin
  coreModule.io.evalDout := evalCoreSram.io.dout

  coreInterSram.io.we := coreModule.io.coreWe
  coreInterSram.io.re := interModule.io.coreRe
  coreInterSram.io.page := Mux(state === sInter, interModule.io.corePage, coreModule.io.corePage)
  coreInterSram.io.bank := Mux(state === sInter, interModule.io.coreBank, coreModule.io.coreBank)
  coreInterSram.io.din := coreModule.io.coreDin
  interModule.io.coreDout := coreInterSram.io.dout

  switch(state) {
    is(sIdle) {
      when(io.valid_in) { state := sLoadInput }
    }
    is(sLoadInput) {
      inputA := io.a
      inputB := io.b
      evalModule.io.start := true.B
      state := sEval
    }
    is(sEval) {
      when(evalModule.io.done) {
        assert(evalModule.io.debugEvalJobCount === 343.U, "top observed unexpected eval job count")
        coreModule.io.start := true.B
        state := sCore
      }
    }
    is(sCore) {
      when(coreModule.io.done) {
        assert(coreModule.io.debugCoreJobCount === 343.U, "top observed unexpected core job count")
        interModule.io.start := true.B
        state := sInter
      }
    }
    is(sInter) {
      when(interModule.io.done) {
        assert(interModule.io.debugInterLayer1GroupCount === 49.U, "top observed unexpected interpolation layer 1 count")
        assert(interModule.io.debugInterLayer2GroupCount === 7.U, "top observed unexpected interpolation layer 2 count")
        assert(interModule.io.debugInterLayer3GroupCount === 1.U, "top observed unexpected interpolation layer 3 count")
        state := sDone
      }
    }
    is(sDone) { state := sIdle }
  }
}
