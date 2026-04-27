package poly_mult

import chisel3._
import chisel3.util._


object Util {
  def mask(x: UInt, bits: Int): UInt = {
    require(bits > 0, "mask bits must be positive")
    x.pad(bits)(bits - 1, 0)
  }
}
import Util._

// =============================================================================
//  插值参数表：编译期常量
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

  private val w = outW

  val r0 = io.r(0)
  val r1 = io.r(1)
  val r2 = io.r(2)
  val r3 = io.r(3)

  val t0 = mask(r0 + r2, w)
  val t1 = mask(r1 + r3, w)

  io.out(0) := mask(r3, w)
  io.out(1) := mask((r3 << 3) + (r2 << 2) + (r1 << 1) + r0, w)
  io.out(2) := mask(t0 + t1, w)
  io.out(3) := mask(t0 - t1, w)
  io.out(4) := mask((mask((r0 << 2) + r2, w) << 1) + ((r1 << 2) + r3), w)
  io.out(5) := mask((mask((r0 << 2) + r2, w) << 1) - ((r1 << 2) + r3), w)
  io.out(6) := mask(r0, w)
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

  private val w = outW

  val r0 = mask(io.r(0), w)
  val r1 = mask(io.r(1), w)
  val r2 = mask(io.r(2), w)
  val r3 = mask(io.r(3), w)

  val t0 = mask(r0 + r2, w)
  val t1 = mask(r1 + r3, w)

  io.out := MuxLookup(io.pt, 0.U(w.W))(Seq(
    0.U -> mask(r3, w),
    1.U -> mask((r3 << 3) + (r2 << 2) + (r1 << 1) + r0, w),
    2.U -> mask(t0 + t1, w),
    3.U -> mask(t0 - t1, w),
    4.U -> mask((mask((r0 << 2) + r2, w) << 1) + ((r1 << 2) + r3), w),
    5.U -> mask((mask((r0 << 2) + r2, w) << 1) - ((r1 << 2) + r3), w),
    6.U -> mask(r0, w)
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
//  Product4TC43：4系数 × 4系数 -> 7系数，纯组合硬件模块
// =============================================================================
class Product4TC43 extends Module {
  val io = IO(new Bundle {
    val a4  = Input(Vec(4, UInt(24.W)))
    val b4  = Input(Vec(4, UInt(8.W)))
    val out = Output(Vec(7, UInt(36.W)))
  })

  val evalA = Module(new EvalLayerTC43(24, 39))
  val evalB = Module(new EvalLayerTC43(8, 29))

  evalA.io.r := io.a4
  evalB.io.r := io.b4

  val wMul = Wire(Vec(7, UInt(39.W)))

  for (i <- 0 until 7) {
    val bw     = evalB.io.out(i)(28, 0)
    val bwSign = bw(28)
    val bwSext = Cat(Fill(10, bwSign), bw).asSInt
    val awInt  = evalA.io.out(i)(38, 0).asSInt
    wMul(i) := mask((awInt * bwSext).asUInt, 39)
  }

  val r5a = mask(wMul(5) - wMul(4), 39)
  val r3a = mask(mask(wMul(3) - wMul(2), 39) >> 1, 39)
  val r4a = mask(wMul(4) - wMul(0), 39)
  val r4b = mask((r4a << 1) + r5a - (wMul(6) << 7), 39)
  val r2a = mask(wMul(2) + r3a, 39)
  val r1a = mask(wMul(1) + wMul(4) - (r2a << 6) - r2a, 39)
  val r2b = mask(r2a - wMul(6) - wMul(0), 39)
  val r1b = mask(r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), 39)

  val r4c = mask(
    mask(mask(r4b - (r2b << 3), 39) >> 3, 39) * "hAAAAAAAAB".U(42.W), 36
  )
  val r5b = mask(
    mask((r5a + r1b) >> 1, 39) * "hEEEEEEEEF".U(42.W), 37
  )
  val r1c = mask(
    mask(mask(r1b + (r3a << 4), 39) >> 1, 39) * "hE38E38E39".U(42.W), 37
  )

  val r2c = mask(r2b - r4c, 36)
  val r3b = mask(0.U - r3a - r1c, 36)
  val r5c = mask((r1c - r5b) >> 1, 36)
  val r1d = mask(r1c - r5c, 36)

  io.out(0) := mask(wMul(6) - r2c, 36)
  io.out(1) := mask(r5c - r1d, 36)
  io.out(2) := mask(r4c - wMul(0), 36)
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
  val io = IO(new Bundle {
    val valid_in  = Input(Bool())
    val avec      = Input(Vec(16, UInt(24.W)))
    val bvec      = Input(Vec(16, UInt(8.W)))
    val valid_out = Output(Bool())
    val cOut      = Output(Vec(16, UInt(36.W)))
  })

  val ae = Wire(Vec(7 * 4, UInt(39.W)))
  val be = Wire(Vec(7 * 4, UInt(29.W)))

  for (seg <- 0 until 4) {
    val evalA = Module(new EvalLayerTC43(24, 39))
    val evalB = Module(new EvalLayerTC43(8, 29))

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

  val wProd = Wire(Vec(7 * 4, UInt(36.W)))

  for (pt <- 0 until 7) {
    val prod = Module(new Product4TC43)

    for (k <- 0 until 4) {
      prod.io.a4(k) := ae(pt * 4 + k)(23, 0)
      prod.io.b4(k) := be(pt * 4 + k)(7, 0)
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
class ToomCook43IO extends Bundle {
  val valid_in  = Input(Bool())
  val a         = Input(Vec(1024, UInt(24.W)))
  val b         = Input(Vec(1024, UInt(8.W)))
  val valid_out = Output(Bool())
  val c         = Output(Vec(1024, UInt(24.W)))
}

class ToomCook43 extends Module {
  val io = IO(new ToomCook43IO)

  // ==========================================================================
  //  状态机：
  //  IDLE      : 等待 valid_in，并锁存输入 a/b
  //  RUN_CORE  : 循环 49 组，每组驱动 7 个 Core16
  //  WAIT_CORE : 等待最后一组 Core16 的 1 拍延迟输出写回 w2Reg
  //  INTERP1   : 执行 stride=16 插值并寄存
  //  INTERP2   : 执行 stride=64 插值并寄存
  //  INTERP3   : 执行 stride=256 插值并寄存最终结果
  //  DONE      : valid_out 拉高 1 拍
  // ==========================================================================
  object State extends ChiselEnum {
    val IDLE, RUN_CORE, WAIT_CORE, INTERP1, INTERP2, INTERP3, DONE = Value
  }

  val state = RegInit(State.IDLE)

  // 输入锁存寄存器（仅在 IDLE 且 valid_in 时更新）
  val regA = Reg(Vec(1024, UInt(24.W)))
  val regB = Reg(Vec(1024, UInt(8.W)))

  // Stage0 点乘结果存储：343 * 16
  val w2Reg = Reg(Vec(343 * 16, UInt(36.W)))
  val regW1 = Reg(Vec(49 * 64, UInt(33.W)))
  val regW0 = Reg(Vec(7 * 256, UInt(27.W)))
  val regC  = Reg(Vec(1024, UInt(24.W)))

  // RUN_CORE 组计数器：0..48
  val groupCnt = RegInit(0.U(6.W))

  // 仅实例化 7 个 Core16，按 lane 复用
  val cores = Seq.fill(7)(Module(new Core16TC43))
  val buildA = Seq.fill(7)(Module(new BuildEvalVec16(memW = 24, outW = 24)))
  val buildB = Seq.fill(7)(Module(new BuildEvalVec16(memW = 8, outW = 8)))

  // 写地址对齐寄存：Core16 有 1 拍 valid 延迟，因此 seg 也打一拍
  val segPipe  = Reg(Vec(7, UInt(9.W)))
  val segVPipe = RegInit(VecInit(Seq.fill(7)(false.B)))

  val runCoreFire = state === State.RUN_CORE

  for (lane <- 0 until 7) {
    val seg = groupCnt * 7.U + lane.U

    // 注意：pt0/pt1/pt2 都是运行时 UInt，而不是 Scala Int
    val pt0 = (seg / 49.U)(2, 0)
    val pt1 = ((seg / 7.U) % 7.U)(2, 0)
    val pt2 = (seg % 7.U)(2, 0)

    buildA(lane).io.in  := regA
    buildA(lane).io.pt0 := pt0
    buildA(lane).io.pt1 := pt1
    buildA(lane).io.pt2 := pt2

    buildB(lane).io.in  := regB
    buildB(lane).io.pt0 := pt0
    buildB(lane).io.pt1 := pt1
    buildB(lane).io.pt2 := pt2

    cores(lane).io.valid_in := runCoreFire
    cores(lane).io.avec     := buildA(lane).io.out
    cores(lane).io.bvec     := buildB(lane).io.out

    when(runCoreFire) {
      segPipe(lane)  := seg
      segVPipe(lane) := true.B
    }.otherwise {
      segVPipe(lane) := false.B
    }

    // Core16 输出写回 w2Reg，地址使用打一拍后的 seg 对齐 valid
    when(segVPipe(lane) && cores(lane).io.valid_out) {
      for (t <- 0 until 16) {
        val wrIdx = (segPipe(lane) << 4) + t.U
        w2Reg(wrIdx) := cores(lane).io.cOut(t)
      }
    }
  }

  // ==========================================================================
  //  INTERP1：49 个 stride=16 插值（组合），在状态 INTERP1 时寄存
  // ==========================================================================
  val interp1 = Seq.fill(49)(Module(new InterpLayerTC43(stride = 16, pidx = 1, inW = 36, outW = 33)))
  val w1Wire  = Wire(Vec(49 * 64, UInt(33.W)))

  for (seg49 <- 0 until 49) {
    for (k <- 0 until 7 * 16) {
      interp1(seg49).io.wIn(k) := w2Reg(seg49 * 7 * 16 + k)
    }
    for (k <- 0 until 64) {
      w1Wire(seg49 * 64 + k) := interp1(seg49).io.cOut(k)
    }
  }

  // ==========================================================================
  //  INTERP2：7 个 stride=64 插值（组合），在状态 INTERP2 时寄存
  // ==========================================================================
  val interp2 = Seq.fill(7)(Module(new InterpLayerTC43(stride = 64, pidx = 2, inW = 33, outW = 27)))
  val w0Wire  = Wire(Vec(7 * 256, UInt(27.W)))

  for (i <- 0 until 7) {
    for (k <- 0 until 7 * 64) {
      interp2(i).io.wIn(k) := regW1(i * 7 * 64 + k)
    }
    for (k <- 0 until 256) {
      w0Wire(i * 256 + k) := interp2(i).io.cOut(k)
    }
  }

  // ==========================================================================
  //  INTERP3：1 个 stride=256 插值（组合），在状态 INTERP3 时寄存
  // ==========================================================================
  val interpFinal = Module(new InterpLayerTC43(stride = 256, pidx = 3, inW = 27, outW = 24))
  for (k <- 0 until 7 * 256) {
    interpFinal.io.wIn(k) := regW0(k)
  }

  val cWire = Wire(Vec(1024, UInt(24.W)))
  for (i <- 0 until 1024) {
    cWire(i) := mask(interpFinal.io.cOut(i), 24)
  }

  // 默认输出
  io.valid_out := false.B
  io.c         := regC

  // ==========================================================================
  //  FSM 状态转移与寄存控制
  // ==========================================================================
  switch(state) {
    is(State.IDLE) {
      groupCnt := 0.U
      when(io.valid_in) {
        regA  := io.a
        regB  := io.b
        state := State.RUN_CORE
      }
    }

    is(State.RUN_CORE) {
      when(groupCnt === 48.U) {
        state := State.WAIT_CORE
      }.otherwise {
        groupCnt := groupCnt + 1.U
      }
    }

    is(State.WAIT_CORE) {
      // 等待最后一组 core 输出在本拍写回 w2Reg
      state := State.INTERP1
    }

    is(State.INTERP1) {
      regW1 := w1Wire
      state := State.INTERP2
    }

    is(State.INTERP2) {
      regW0 := w0Wire
      state := State.INTERP3
    }

    is(State.INTERP3) {
      regC  := cWire
      state := State.DONE
    }

    is(State.DONE) {
      io.valid_out := true.B
      state := State.IDLE
    }
  }
}