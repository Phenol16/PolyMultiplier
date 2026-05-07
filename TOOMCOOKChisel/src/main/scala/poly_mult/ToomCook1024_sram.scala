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
//  EvalFull16TC4：默认 1024 点输入的三层 TC4 全展开求值
//  每拍根据 pt0/pt1/pt2 直接生成 16 个 Core16 输入元素，不再使用旧 lane/phase 拼接。
// =============================================================================
class EvalFull16TC4 extends Module {
  private val N = 1024
  private val A_IN_W = 24
  private val B_IN_W = 8
  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W
  private val CORE_SIZE = 16

  val io = IO(new Bundle {
    val a = Input(Vec(N, UInt(A_IN_W.W)))
    val b = Input(Vec(N, UInt(B_IN_W.W)))

    val pt0 = Input(UInt(3.W))
    val pt1 = Input(UInt(3.W))
    val pt2 = Input(UInt(3.W))

    val aOut = Output(Vec(CORE_SIZE, UInt(A_EVAL_W.W)))
    val bOut = Output(Vec(CORE_SIZE, UInt(B_EVAL_W.W)))
  })

  private def eval64(
      in: Vec[UInt],
      inW: Int,
      outW: Int,
      base: Int
  ): UInt = {
    val level2 = Wire(Vec(4, UInt(outW.W)))

    for (k <- 0 until 4) {
      val level1 = Wire(Vec(4, UInt(outW.W)))

      for (j <- 0 until 4) {
        val eval0 = Module(new TC4EvalPoint(inW, outW))
        val offset = base + 16 * k + 4 * j

        eval0.io.r(0) := in(offset + 0)
        eval0.io.r(1) := in(offset + 1)
        eval0.io.r(2) := in(offset + 2)
        eval0.io.r(3) := in(offset + 3)
        eval0.io.pt := io.pt0
        level1(j) := eval0.io.out
      }

      val eval1 = Module(new TC4EvalPoint(outW, outW))
      eval1.io.r := level1
      eval1.io.pt := io.pt1
      level2(k) := eval1.io.out
    }

    val eval2 = Module(new TC4EvalPoint(outW, outW))
    eval2.io.r := level2
    eval2.io.pt := io.pt2
    eval2.io.out
  }

  for (i <- 0 until CORE_SIZE) {
    io.aOut(i) := eval64(io.a, A_IN_W, A_EVAL_W, i * 64)
    io.bOut(i) := eval64(io.b, B_IN_W, B_EVAL_W, i * 64)
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
//  final stage 由顶层从 interp64OutRam 每拍读 7 个 54-bit pair 后喂入本模块。
//  输出缓存保留在模块内部，便于不改变 ToomCook43.io.c 的 Vec(1024, UInt(24.W)) 接口。
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

class ToomCook43IO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(1024, UInt(24.W)))
  val b = Input(Vec(1024, UInt(8.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(1024, UInt(24.W)))
}

// =============================================================================
//  ToomCook43 顶层：Eval -> Eval/Core SRAM -> Core -> Core/Interp SRAM -> Interp
//  第一阶段固定默认配置，重点是清晰的帧级三阶段流水和简单 banked SRAM。
// =============================================================================
class ToomCook43 extends Module {
  // ---------------------------------------------------------------------------
  // Local helpers
  // ---------------------------------------------------------------------------
  private def packVec(xs: Seq[UInt]): UInt = Cat(xs.reverse)

  private def constMul7(x: UInt): UInt = (x << 2) +& (x << 1) +& x

  private def constMul49(x: UInt): UInt = (x << 5) +& (x << 4) +& x

  private def evalAddrFromPoints(pt0: UInt, pt1: UInt, pt2: UInt): UInt = {
    mask(constMul49(pt0) +& constMul7(pt1) +& pt2, EVAL_ADDR_W)
  }

  private def firstBankWithState(states: Vec[UInt], target: UInt): (Bool, UInt) = {
    val hits = VecInit((0 until FRAME_BANKS).map(i => states(i) === target))
    (hits.asUInt.orR, PriorityEncoder(hits))
  }

  // ---------------------------------------------------------------------------
  // IO/constants
  // ---------------------------------------------------------------------------
  val io = IO(new ToomCook43IO)

  private val N = 1024
  private val A_IN_W = 24
  private val B_IN_W = 8
  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W
  private val CORE_SIZE = 16
  private val EVAL_JOB_COUNT = 343
  private val CORE_OUT_W = 36
  private val INTERP16_OUT_W = 33
  private val INTERP64_OUT_W = 27
  private val FINAL_OUT_W = 24
  private val FRAME_BANKS = 2
  private val EVAL_ADDR_W = log2Ceil(EVAL_JOB_COUNT)

  val bankEmpty :: bankWriting :: bankFull :: bankReading :: Nil = Enum(4)

  // ---------------------------------------------------------------------------
  // Registers
  // ---------------------------------------------------------------------------
  val regA = Reg(Vec(N, UInt(A_IN_W.W)))
  val regB = Reg(Vec(N, UInt(B_IN_W.W)))
  val finalOutReg = RegInit(VecInit(Seq.fill(N)(0.U(FINAL_OUT_W.W))))

  val evalCoreBankState = RegInit(VecInit(Seq.fill(FRAME_BANKS)(bankEmpty)))
  val coreInterpBankState = RegInit(VecInit(Seq.fill(FRAME_BANKS)(bankEmpty)))

  val evalIdle :: evalRun :: Nil = Enum(2)
  val evalState = RegInit(evalIdle)
  val evalWriteBank = RegInit(0.U(log2Ceil(FRAME_BANKS).W))
  val evalAddr = RegInit(0.U(EVAL_ADDR_W.W))
  val evalPt0 = RegInit(0.U(3.W))
  val evalPt1 = RegInit(0.U(3.W))
  val evalPt2 = RegInit(0.U(3.W))

  val coreIdle :: coreReadReq :: coreStream :: coreDrain :: Nil = Enum(4)
  val coreState = RegInit(coreIdle)
  val coreReadBank = RegInit(0.U(log2Ceil(FRAME_BANKS).W))
  val coreWriteBank = RegInit(0.U(log2Ceil(FRAME_BANKS).W))
  val coreReadAddr = RegInit(0.U(EVAL_ADDR_W.W))
  val coreRowsFed = RegInit(0.U(log2Ceil(EVAL_JOB_COUNT + 1).W))
  val coreWriteAddr = RegInit(0.U(EVAL_ADDR_W.W))

  val interpIdle :: interp16ReadReq :: interp16ReadCap :: interp16Start :: interp16Run ::
    interp64Start :: interp64Run :: interp256Start :: interp256ReadPipe :: interp256Run :: Nil = Enum(10)
  val interpState = RegInit(interpIdle)
  val interpReadBank = RegInit(0.U(log2Ceil(FRAME_BANKS).W))
  val interpPt0 = RegInit(0.U(3.W))
  val interpPt1 = RegInit(0.U(3.W))
  val interpPt2 = RegInit(0.U(3.W))
  val interp16InBuf = Reg(Vec(7, Vec(CORE_SIZE, UInt(CORE_OUT_W.W))))
  val interp16CapturePt = RegInit(0.U(3.W))
  val interp16ReadAddr = RegInit(0.U(EVAL_ADDR_W.W))
  val interp16OutPt0 = RegInit(0.U(3.W))
  val interp16OutPt1 = RegInit(0.U(3.W))
  val interp64ReadCol = RegInit(0.U(7.W))
  val interp64FeedValid = RegInit(false.B)
  val interp64FeedSel = RegInit(0.U(2.W))
  val interp64WritePt0 = RegInit(0.U(3.W))
  val interp256ReadPairIdx = RegInit(0.U(8.W))
  val interp256FeedValid = RegInit(false.B)
  val interp256FeedSel = RegInit(false.B)

  // ---------------------------------------------------------------------------
  // SRAM banks
  // ---------------------------------------------------------------------------
  val evalARam = Seq.fill(FRAME_BANKS, CORE_SIZE)(Module(new SpRam(A_EVAL_W, EVAL_JOB_COUNT)))
  val evalBRam = Seq.fill(FRAME_BANKS, CORE_SIZE)(Module(new SpRam(B_EVAL_W, EVAL_JOB_COUNT)))
  val coreOutRam = Seq.fill(FRAME_BANKS, CORE_SIZE)(Module(new SpRam(CORE_OUT_W, EVAL_JOB_COUNT)))

  // interp16OutRam(pt0)(pt1)：每个 word 打包 4 个 33-bit 系数。
  val interp16OutRam = Seq.fill(7, 7)(Module(new SpRam(4 * INTERP16_OUT_W, 16)))

  // interp64OutRam(pt0)：每个 word 打包 4 个 27-bit 系数。
  val interp64OutRam = Seq.fill(7)(Module(new SpRam(4 * INTERP64_OUT_W, 64)))

  // ---------------------------------------------------------------------------
  // Submodules
  // ---------------------------------------------------------------------------
  val evalFull = Module(new EvalFull16TC4)
  val core = Module(new Core16TC4)
  val interp16Seq = Module(new InterpLayerSeqStreamTC4(16, 1, CORE_OUT_W, INTERP16_OUT_W))
  val interp64Seq = Module(new InterpLayerSeqStreamInOutTC4(64, 2, INTERP16_OUT_W, INTERP64_OUT_W))
  val interp256Seq = Module(new InterpLayerSeq2ColStreamInTC4(256, 3, INTERP64_OUT_W, FINAL_OUT_W))

  // ---------------------------------------------------------------------------
  // Default assignments
  // ---------------------------------------------------------------------------
  io.valid_out := false.B
  io.c := finalOutReg

  evalFull.io.a := regA
  evalFull.io.b := regB
  evalFull.io.pt0 := evalPt0
  evalFull.io.pt1 := evalPt1
  evalFull.io.pt2 := evalPt2

  core.io.valid_in := false.B
  for (i <- 0 until CORE_SIZE) {
    core.io.avec(i) := Mux1H(
      (0 until FRAME_BANKS).map(b => (coreReadBank === b.U) -> evalARam(b)(i).io.dout)
    )
    core.io.bvec(i) := Mux1H(
      (0 until FRAME_BANKS).map(b => (coreReadBank === b.U) -> evalBRam(b)(i).io.dout)
    )
  }

  interp16Seq.io.start := false.B
  for (pt <- 0 until 7; col <- 0 until CORE_SIZE) {
    interp16Seq.io.wIn(pt * CORE_SIZE + col) := interp16InBuf(pt)(col)
  }

  interp64Seq.io.start := false.B
  interp64Seq.io.inValid := false.B
  interp64Seq.io.inData := VecInit(Seq.fill(7)(0.U(INTERP16_OUT_W.W)))

  interp256Seq.io.start := false.B
  interp256Seq.io.inValid := false.B
  interp256Seq.io.inPair := VecInit(Seq.fill(7)(0.U((2 * INTERP64_OUT_W).W)))

  for (b <- 0 until FRAME_BANKS; i <- 0 until CORE_SIZE) {
    evalARam(b)(i).io.clk := clock
    evalARam(b)(i).io.en := false.B
    evalARam(b)(i).io.we := false.B
    evalARam(b)(i).io.addr := 0.U
    evalARam(b)(i).io.din := 0.U

    evalBRam(b)(i).io.clk := clock
    evalBRam(b)(i).io.en := false.B
    evalBRam(b)(i).io.we := false.B
    evalBRam(b)(i).io.addr := 0.U
    evalBRam(b)(i).io.din := 0.U

    coreOutRam(b)(i).io.clk := clock
    coreOutRam(b)(i).io.en := false.B
    coreOutRam(b)(i).io.we := false.B
    coreOutRam(b)(i).io.addr := 0.U
    coreOutRam(b)(i).io.din := 0.U
  }

  for (pt0 <- 0 until 7; pt1 <- 0 until 7) {
    interp16OutRam(pt0)(pt1).io.clk := clock
    interp16OutRam(pt0)(pt1).io.en := false.B
    interp16OutRam(pt0)(pt1).io.we := false.B
    interp16OutRam(pt0)(pt1).io.addr := 0.U
    interp16OutRam(pt0)(pt1).io.din := 0.U
  }

  for (pt0 <- 0 until 7) {
    interp64OutRam(pt0).io.clk := clock
    interp64OutRam(pt0).io.en := false.B
    interp64OutRam(pt0).io.we := false.B
    interp64OutRam(pt0).io.addr := 0.U
    interp64OutRam(pt0).io.din := 0.U
  }

  // ---------------------------------------------------------------------------
  // EvalStage
  // ---------------------------------------------------------------------------
  val (hasEmptyEvalCoreBank, nextEvalWriteBank) = firstBankWithState(evalCoreBankState, bankEmpty)

  when(evalState === evalIdle) {
    when(io.valid_in && hasEmptyEvalCoreBank) {
      regA := io.a
      regB := io.b
      evalWriteBank := nextEvalWriteBank
      evalCoreBankState(nextEvalWriteBank) := bankWriting
      evalAddr := 0.U
      evalPt0 := 0.U
      evalPt1 := 0.U
      evalPt2 := 0.U
      evalState := evalRun
    }
  }.elsewhen(evalState === evalRun) {
    for (b <- 0 until FRAME_BANKS; i <- 0 until CORE_SIZE) {
      when(evalWriteBank === b.U) {
        evalARam(b)(i).io.en := true.B
        evalARam(b)(i).io.we := true.B
        evalARam(b)(i).io.addr := evalAddr
        evalARam(b)(i).io.din := evalFull.io.aOut(i)

        evalBRam(b)(i).io.en := true.B
        evalBRam(b)(i).io.we := true.B
        evalBRam(b)(i).io.addr := evalAddr
        evalBRam(b)(i).io.din := evalFull.io.bOut(i)
      }
    }

    when(evalAddr === (EVAL_JOB_COUNT - 1).U) {
      evalCoreBankState(evalWriteBank) := bankFull
      evalState := evalIdle
    }.otherwise {
      evalAddr := evalAddr + 1.U
      when(evalPt2 === 6.U) {
        evalPt2 := 0.U
        when(evalPt1 === 6.U) {
          evalPt1 := 0.U
          evalPt0 := evalPt0 + 1.U
        }.otherwise {
          evalPt1 := evalPt1 + 1.U
        }
      }.otherwise {
        evalPt2 := evalPt2 + 1.U
      }
    }
  }

  // ---------------------------------------------------------------------------
  // CoreStage
  // 第一版为保证正确性，仅做一条简单的同步 SRAM 读流水：每拍消费上一拍 dout，
  // 同时（若有剩余行）发起下一行读；Core16TC4 自带 valid 对齐输出。
  // ---------------------------------------------------------------------------
  val (hasFullEvalCoreBank, nextCoreReadBank) = firstBankWithState(evalCoreBankState, bankFull)
  val (hasEmptyCoreInterpBank, nextCoreWriteBank) = firstBankWithState(coreInterpBankState, bankEmpty)

  when(coreState === coreIdle) {
    when(hasFullEvalCoreBank && hasEmptyCoreInterpBank) {
      coreReadBank := nextCoreReadBank
      coreWriteBank := nextCoreWriteBank
      evalCoreBankState(nextCoreReadBank) := bankReading
      coreInterpBankState(nextCoreWriteBank) := bankWriting
      coreReadAddr := 0.U
      coreRowsFed := 0.U
      coreWriteAddr := 0.U
      coreState := coreReadReq
    }
  }.elsewhen(coreState === coreReadReq) {
    for (b <- 0 until FRAME_BANKS; i <- 0 until CORE_SIZE) {
      when(coreReadBank === b.U) {
        evalARam(b)(i).io.en := true.B
        evalARam(b)(i).io.we := false.B
        evalARam(b)(i).io.addr := 0.U

        evalBRam(b)(i).io.en := true.B
        evalBRam(b)(i).io.we := false.B
        evalBRam(b)(i).io.addr := 0.U
      }
    }
    coreReadAddr := 1.U
    coreState := coreStream
  }.elsewhen(coreState === coreStream) {
    core.io.valid_in := true.B

    when(coreReadAddr < EVAL_JOB_COUNT.U) {
      for (b <- 0 until FRAME_BANKS; i <- 0 until CORE_SIZE) {
        when(coreReadBank === b.U) {
          evalARam(b)(i).io.en := true.B
          evalARam(b)(i).io.we := false.B
          evalARam(b)(i).io.addr := coreReadAddr

          evalBRam(b)(i).io.en := true.B
          evalBRam(b)(i).io.we := false.B
          evalBRam(b)(i).io.addr := coreReadAddr
        }
      }
      coreReadAddr := coreReadAddr + 1.U
    }

    coreRowsFed := coreRowsFed + 1.U
    when(coreRowsFed === (EVAL_JOB_COUNT - 1).U) {
      coreState := coreDrain
    }
  }.elsewhen(coreState === coreDrain) {
    when(core.io.valid_out && coreWriteAddr === (EVAL_JOB_COUNT - 1).U) {
      evalCoreBankState(coreReadBank) := bankEmpty
      coreInterpBankState(coreWriteBank) := bankFull
      coreState := coreIdle
    }
  }

  when(core.io.valid_out) {
    for (b <- 0 until FRAME_BANKS; i <- 0 until CORE_SIZE) {
      when(coreWriteBank === b.U) {
        coreOutRam(b)(i).io.en := true.B
        coreOutRam(b)(i).io.we := true.B
        coreOutRam(b)(i).io.addr := coreWriteAddr
        coreOutRam(b)(i).io.din := core.io.cOut(i)
      }
    }

    when(coreWriteAddr =/= (EVAL_JOB_COUNT - 1).U) {
      coreWriteAddr := coreWriteAddr + 1.U
    }
  }

  // ---------------------------------------------------------------------------
  // InterpStage
  // 从 Core/Interp bank 读取完整一帧：先按 (pt0, pt1) 聚齐 7 个 pt2 行，
  // 依次完成 16/64/256 三层流式插值，并在完成后释放 Core/Interp bank。
  // ---------------------------------------------------------------------------
  val (hasFullCoreInterpBank, nextInterpReadBank) = firstBankWithState(coreInterpBankState, bankFull)

  when(interpState === interpIdle) {
    when(hasFullCoreInterpBank) {
      interpReadBank := nextInterpReadBank
      coreInterpBankState(nextInterpReadBank) := bankReading
      interpPt0 := 0.U
      interpPt1 := 0.U
      interpPt2 := 0.U
      interpState := interp16ReadReq
    }
  }.elsewhen(interpState === interp16ReadReq) {
    interp16ReadAddr := evalAddrFromPoints(interpPt0, interpPt1, interpPt2)
    interp16CapturePt := interpPt2

    for (b <- 0 until FRAME_BANKS; i <- 0 until CORE_SIZE) {
      when(interpReadBank === b.U) {
        coreOutRam(b)(i).io.en := true.B
        coreOutRam(b)(i).io.we := false.B
        coreOutRam(b)(i).io.addr := evalAddrFromPoints(interpPt0, interpPt1, interpPt2)
      }
    }
    interpState := interp16ReadCap
  }.elsewhen(interpState === interp16ReadCap) {
    for (i <- 0 until CORE_SIZE) {
      interp16InBuf(interp16CapturePt)(i) := Mux1H(
        (0 until FRAME_BANKS).map(b => (interpReadBank === b.U) -> coreOutRam(b)(i).io.dout)
      )
    }

    when(interpPt2 === 6.U) {
      interp16OutPt0 := interpPt0
      interp16OutPt1 := interpPt1
      interpState := interp16Start
    }.otherwise {
      interpPt2 := interpPt2 + 1.U
      interpState := interp16ReadReq
    }
  }.elsewhen(interpState === interp16Start) {
    interp16Seq.io.start := true.B
    interpState := interp16Run
  }.elsewhen(interpState === interp16Run) {
    when(interp16Seq.io.outValid) {
      for (pt0 <- 0 until 7; pt1 <- 0 until 7) {
        when(interp16OutPt0 === pt0.U && interp16OutPt1 === pt1.U) {
          interp16OutRam(pt0)(pt1).io.en := true.B
          interp16OutRam(pt0)(pt1).io.we := true.B
          interp16OutRam(pt0)(pt1).io.addr := (interp16Seq.io.outBase >> 2)(3, 0)
          interp16OutRam(pt0)(pt1).io.din := packVec(interp16Seq.io.outData)
        }
      }
    }

    when(interp16Seq.io.done) {
      when(interpPt1 === 6.U) {
        interp64WritePt0 := interpPt0
        interpState := interp64Start
      }.otherwise {
        interpPt1 := interpPt1 + 1.U
        interpPt2 := 0.U
        interpState := interp16ReadReq
      }
    }
  }.elsewhen(interpState === interp64Start) {
    interp64Seq.io.start := true.B

    for (pt0 <- 0 until 7; sub <- 0 until 7) {
      when(interp64WritePt0 === pt0.U) {
        interp16OutRam(pt0)(sub).io.en := true.B
        interp16OutRam(pt0)(sub).io.we := false.B
        interp16OutRam(pt0)(sub).io.addr := 0.U
      }
    }

    interp64ReadCol := 1.U
    interp64FeedValid := true.B
    interp64FeedSel := 0.U
    interpState := interp64Run
  }.elsewhen(interpState === interp64Run) {
    when(interp64FeedValid) {
      interp64Seq.io.inValid := true.B
      for (sub <- 0 until 7) {
        val word = Mux1H(
          (0 until 7).map(pt0 => (interp64WritePt0 === pt0.U) -> interp16OutRam(pt0)(sub).io.dout)
        )
        interp64Seq.io.inData(sub) := MuxLookup(interp64FeedSel, word(32, 0))(Seq(
          0.U -> word(32, 0),
          1.U -> word(65, 33),
          2.U -> word(98, 66),
          3.U -> word(131, 99)
        ))
      }
    }

    when(interp64ReadCol === 64.U) {
      interp64FeedValid := false.B
    }.otherwise {
      for (pt0 <- 0 until 7; sub <- 0 until 7) {
        when(interp64WritePt0 === pt0.U) {
          interp16OutRam(pt0)(sub).io.en := true.B
          interp16OutRam(pt0)(sub).io.we := false.B
          interp16OutRam(pt0)(sub).io.addr := (interp64ReadCol >> 2)(3, 0)
        }
      }
      interp64FeedSel := interp64ReadCol(1, 0)
      interp64ReadCol := interp64ReadCol + 1.U
    }

    when(interp64Seq.io.outValid) {
      for (pt0 <- 0 until 7) {
        when(interp64WritePt0 === pt0.U) {
          interp64OutRam(pt0).io.en := true.B
          interp64OutRam(pt0).io.we := true.B
          interp64OutRam(pt0).io.addr := (interp64Seq.io.outBase >> 2)(5, 0)
          interp64OutRam(pt0).io.din := packVec(interp64Seq.io.outData)
        }
      }
    }

    when(interp64Seq.io.done) {
      when(interp64WritePt0 === 6.U) {
        interpState := interp256Start
      }.otherwise {
        interpPt0 := interp64WritePt0 + 1.U
        interpPt1 := 0.U
        interpPt2 := 0.U
        interpState := interp16ReadReq
      }
    }
  }.elsewhen(interpState === interp256Start) {
    interp256Seq.io.start := true.B

    for (pt0 <- 0 until 7) {
      interp64OutRam(pt0).io.en := true.B
      interp64OutRam(pt0).io.we := false.B
      interp64OutRam(pt0).io.addr := 0.U
    }

    interp256ReadPairIdx := 1.U
    interp256FeedValid := true.B
    interp256FeedSel := false.B
    interpState := interp256ReadPipe
  }.elsewhen(interpState === interp256ReadPipe) {
    when(interp256FeedValid) {
      interp256Seq.io.inValid := true.B
      for (pt0 <- 0 until 7) {
        val word = interp64OutRam(pt0).io.dout
        interp256Seq.io.inPair(pt0) := Mux(interp256FeedSel, word(107, 54), word(53, 0))
      }
    }

    when(interp256ReadPairIdx === 128.U) {
      interp256FeedValid := false.B
      interpState := interp256Run
    }.otherwise {
      for (pt0 <- 0 until 7) {
        interp64OutRam(pt0).io.en := true.B
        interp64OutRam(pt0).io.we := false.B
        interp64OutRam(pt0).io.addr := (interp256ReadPairIdx >> 1)(5, 0)
      }
      interp256FeedSel := interp256ReadPairIdx(0)
      interp256ReadPairIdx := interp256ReadPairIdx + 1.U
    }
  }.elsewhen(interpState === interp256Run) {
    when(interp256Seq.io.done) {
      for (i <- 0 until N) {
        finalOutReg(i) := mask(interp256Seq.io.cOut(i), FINAL_OUT_W)
      }
      io.valid_out := true.B
      coreInterpBankState(interpReadBank) := bankEmpty
      interpState := interpIdle
    }
  }

  // ---------------------------------------------------------------------------
  // Output logic
  // ---------------------------------------------------------------------------
  // io.valid_out 仅在最终插值完成的周期拉高一拍；io.c 保持最近一帧结果。
}
