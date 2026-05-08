package poly_mult_sram

import chisel3._
import chisel3.util._

object Util {
  def mask(value: UInt, targetWidth: Int): UInt = {
    require(targetWidth > 0, "mask width must be positive")
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, 0.U), value)
  }

  def fillMsb(value: UInt, targetWidth: Int): UInt = {
    require(targetWidth > 0, "fillMsb width must be positive")
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
  }
}
import Util._

object TC4EvalWidth {
  val A_EVAL_W = 39
  val B_EVAL_W = 29
}

object InterpParamTable {
  case class Param(mk: Int, mk2: Int, mk3: Int, inv3: BigInt, inv9: BigInt, inv18: BigInt)

  val params = Seq(
    Param(36, 33, 34, BigInt("AAAAAAAAB", 16), BigInt("238E38E39", 16), BigInt("2EEEEEEEF", 16)),
    Param(33, 30, 31, BigInt("2AAAAAAB", 16), BigInt("38E38E39", 16), BigInt("6EEEEEEF", 16)),
    Param(30, 27, 28, BigInt("2AAAAAB", 16), BigInt("8E38E39", 16), BigInt("EEEEEEF", 16)),
    Param(27, 24, 25, BigInt("AAAAAB", 16), BigInt("E38E39", 16), BigInt("EEEEEEF", 16))
  )
}

class EvalLayerTC4(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r = Input(Vec(4, UInt(inW.W)))
    val out = Output(Vec(7, UInt(outW.W)))
  })

  val r0 = io.r(0)
  val r1 = io.r(1)
  val r2 = io.r(2)
  val r3 = io.r(3)

  val even = r0 +& r2
  val odd = r1 +& r3
  val scaledEven = Cat(r0, 0.U(2.W)) +& r2
  val scaledOdd = Cat(r1, 0.U(2.W)) +& r3
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

class TC4EvalPoint(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r = Input(Vec(4, UInt(inW.W)))
    val pt = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  val layer = Module(new EvalLayerTC4(inW, outW))
  layer.io.r := io.r
  io.out := MuxLookup(io.pt, 0.U(outW.W))((0 until 7).map(i => i.U -> layer.io.out(i)))
}

class Eval64Point(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(64, UInt(inW.W)))
    val pt0 = Input(UInt(3.W))
    val pt1 = Input(UInt(3.W))
    val pt2 = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  val mid = Wire(Vec(16, UInt(outW.W)))
  for (outer <- 0 until 4) {
    for (middle <- 0 until 4) {
      val eval = Module(new TC4EvalPoint(inW, outW))
      for (inner <- 0 until 4) eval.io.r(inner) := io.in(outer * 16 + middle * 4 + inner)
      eval.io.pt := io.pt0
      mid(outer * 4 + middle) := eval.io.out
    }
  }

  val high = Wire(Vec(4, UInt(outW.W)))
  for (outer <- 0 until 4) {
    val eval = Module(new TC4EvalPoint(outW, outW))
    for (middle <- 0 until 4) eval.io.r(middle) := mid(outer * 4 + middle)
    eval.io.pt := io.pt1
    high(outer) := eval.io.out
  }

  val eval = Module(new TC4EvalPoint(outW, outW))
  eval.io.r := high
  eval.io.pt := io.pt2
  io.out := eval.io.out
}

class InterpCoreTC4(pidx: Int, inW: Int) extends Module {
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
  val r4c = mask(mask(mask(r4b - (r2b << 3), mk) >> 3, mk) * p.inv3.U(42.W), mk2)
  val r5b = mask(mask((r5a + r1b) >> 1, mk) * p.inv18.U(42.W), mk3)
  val r1c = mask(mask(mask(r1b + (r3a << 4), mk) >> 1, mk) * p.inv9.U(42.W), mk3)
  val r2c = mask(r2b - r4c, mk2)
  val r3b = mask(0.U - r3a - r1c, mk2)
  val r5c = mask((r1c - r5b) >> 1, mk2)
  val r1d = mask(r1c - r5c, mk2)

  io.c3 := r3b
  io.c0part := mask(p6 + io.pr2, mk2)
  io.c1part := mask(r5c + io.pr1, mk2)
  io.c2part := mask(r4c + io.pr0, mk2)
  io.nr0 := mask(p0, mk2)
  io.nr1 := r1d
  io.nr2 := r2c
}

class Interp4ColsTC4(pidx: Int, inW: Int, outW: Int) extends Module {
  private val mk2 = InterpParamTable.params(pidx).mk2

  val io = IO(new Bundle {
    val in = Input(Vec(28, UInt(inW.W)))
    val out = Output(Vec(16, UInt(outW.W)))
  })

  val prevR0 = Wire(Vec(5, UInt(mk2.W)))
  val prevR1 = Wire(Vec(5, UInt(mk2.W)))
  val prevR2 = Wire(Vec(5, UInt(mk2.W)))
  val raw = Wire(Vec(16, UInt(outW.W)))
  prevR0(0) := 0.U
  prevR1(0) := 0.U
  prevR2(0) := 0.U

  for (col <- 0 until 4) {
    val core = Module(new InterpCoreTC4(pidx, inW))
    for (pt <- 0 until 7) core.io.pIn(pt) := io.in(pt * 4 + col)
    core.io.pr0 := prevR0(col)
    core.io.pr1 := prevR1(col)
    core.io.pr2 := prevR2(col)

    raw(col * 4 + 0) := mask(core.io.c0part, outW)
    raw(col * 4 + 1) := mask(core.io.c1part, outW)
    raw(col * 4 + 2) := mask(core.io.c2part, outW)
    raw(col * 4 + 3) := mask(core.io.c3, outW)
    prevR0(col + 1) := core.io.nr0
    prevR1(col + 1) := core.io.nr1
    prevR2(col + 1) := core.io.nr2
  }

  io.out := raw
  io.out(0) := mask(raw(0) - prevR2(4), outW)
  io.out(1) := mask(raw(1) - prevR1(4), outW)
  io.out(2) := mask(raw(2) - prevR0(4), outW)
}

class Interp8ColsStepTC4(pidx: Int, inW: Int, outW: Int) extends Module {
  private val mk2 = InterpParamTable.params(pidx).mk2

  val io = IO(new Bundle {
    val in = Input(Vec(7 * 8, UInt(inW.W)))
    val pr0 = Input(UInt(mk2.W))
    val pr1 = Input(UInt(mk2.W))
    val pr2 = Input(UInt(mk2.W))
    val out = Output(Vec(8 * 4, UInt(outW.W)))
    val nr0 = Output(UInt(mk2.W))
    val nr1 = Output(UInt(mk2.W))
    val nr2 = Output(UInt(mk2.W))
  })

  val carry0 = Wire(Vec(9, UInt(mk2.W)))
  val carry1 = Wire(Vec(9, UInt(mk2.W)))
  val carry2 = Wire(Vec(9, UInt(mk2.W)))
  carry0(0) := io.pr0
  carry1(0) := io.pr1
  carry2(0) := io.pr2

  for (col <- 0 until 8) {
    val core = Module(new InterpCoreTC4(pidx, inW))
    for (pt <- 0 until 7) core.io.pIn(pt) := io.in(pt * 8 + col)
    core.io.pr0 := carry0(col)
    core.io.pr1 := carry1(col)
    core.io.pr2 := carry2(col)

    io.out(col * 4 + 0) := mask(core.io.c0part, outW)
    io.out(col * 4 + 1) := mask(core.io.c1part, outW)
    io.out(col * 4 + 2) := mask(core.io.c2part, outW)
    io.out(col * 4 + 3) := mask(core.io.c3, outW)
    carry0(col + 1) := core.io.nr0
    carry1(col + 1) := core.io.nr1
    carry2(col + 1) := core.io.nr2
  }

  io.nr0 := carry0(8)
  io.nr1 := carry1(8)
  io.nr2 := carry2(8)
}

class Product4TC4 extends Module {
  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W
  private val PROD_MUL_MOD_W = A_EVAL_W
  private val PROD_OUT_W = 36

  val io = IO(new Bundle {
    val a4 = Input(Vec(4, UInt(A_EVAL_W.W)))
    val b4 = Input(Vec(4, UInt(B_EVAL_W.W)))
    val out = Output(Vec(7, UInt(PROD_OUT_W.W)))
  })

  val evalA = Module(new EvalLayerTC4(A_EVAL_W, A_EVAL_W))
  val evalB = Module(new EvalLayerTC4(B_EVAL_W, B_EVAL_W))
  evalA.io.r := io.a4
  evalB.io.r := io.b4

  val wMul = Wire(Vec(7, UInt(PROD_MUL_MOD_W.W)))
  for (i <- 0 until 7) {
    val bw = evalB.io.out(i)(B_EVAL_W - 1, 0)
    val bwSext = Cat(Fill(A_EVAL_W - B_EVAL_W, bw(B_EVAL_W - 1)), bw).asSInt
    wMul(i) := mask((evalA.io.out(i)(A_EVAL_W - 1, 0).asSInt * bwSext).asUInt, PROD_MUL_MOD_W)
  }

  val r5a = mask(wMul(5) - wMul(4), PROD_MUL_MOD_W)
  val r3a = mask(mask(wMul(3) - wMul(2), PROD_MUL_MOD_W) >> 1, PROD_MUL_MOD_W)
  val r4a = mask(wMul(4) - wMul(0), PROD_MUL_MOD_W)
  val r4b = mask((r4a << 1) + r5a - (wMul(6) << 7), PROD_MUL_MOD_W)
  val r2a = mask(wMul(2) + r3a, PROD_MUL_MOD_W)
  val r1a = mask(wMul(1) + wMul(4) - (r2a << 6) - r2a, PROD_MUL_MOD_W)
  val r2b = mask(r2a - wMul(6) - wMul(0), PROD_MUL_MOD_W)
  val r1b = mask(r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), PROD_MUL_MOD_W)
  val r4c = mask(mask(mask(r4b - (r2b << 3), PROD_MUL_MOD_W) >> 3, PROD_MUL_MOD_W) * "hAAAAAAAAB".U(42.W), PROD_OUT_W)
  val r5b = mask(mask((r5a + r1b) >> 1, PROD_MUL_MOD_W) * "hEEEEEEEEF".U(42.W), 37)
  val r1c = mask(mask(mask(r1b + (r3a << 4), PROD_MUL_MOD_W) >> 1, PROD_MUL_MOD_W) * "hE38E38E39".U(42.W), 37)
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

class Core16TC4 extends Module {
  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W
  private val CORE_OUT_W = 36

  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val avec = Input(Vec(16, UInt(A_EVAL_W.W)))
    val bvec = Input(Vec(16, UInt(B_EVAL_W.W)))
    val valid_out = Output(Bool())
    val cOut = Output(Vec(16, UInt(CORE_OUT_W.W)))
  })

  val ae = Wire(Vec(28, UInt(A_EVAL_W.W)))
  val be = Wire(Vec(28, UInt(B_EVAL_W.W)))
  for (seg <- 0 until 4) {
    val evalA = Module(new EvalLayerTC4(A_EVAL_W, A_EVAL_W))
    val evalB = Module(new EvalLayerTC4(B_EVAL_W, B_EVAL_W))
    for (k <- 0 until 4) {
      evalA.io.r(k) := io.avec(seg * 4 + k)
      evalB.io.r(k) := io.bvec(seg * 4 + k)
    }
    for (pt <- 0 until 7) {
      ae(pt * 4 + seg) := evalA.io.out(pt)
      be(pt * 4 + seg) := evalB.io.out(pt)
    }
  }

  val wProd = Wire(Vec(28, UInt(CORE_OUT_W.W)))
  for (pt <- 0 until 7) {
    val prod = Module(new Product4TC4)
    for (k <- 0 until 4) {
      prod.io.a4(k) := ae(pt * 4 + k)
      prod.io.b4(k) := be(pt * 4 + k)
      wProd(pt * 4 + k) := prod.io.out(k)
    }
  }

  val regW = RegEnable(wProd, io.valid_in)
  val regValid = RegNext(io.valid_in, false.B)
  val interp = Module(new Interp4ColsTC4(pidx = 0, inW = 36, outW = 36))
  interp.io.in := regW

  io.valid_out := regValid
  io.cOut := interp.io.out
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

class ToomCook43Clean extends Module {
  val io = IO(new ToomCook43IO)

  private val A_EVAL_W = TC4EvalWidth.A_EVAL_W
  private val B_EVAL_W = TC4EvalWidth.B_EVAL_W

  private def packVec(xs: Seq[UInt]): UInt = Cat(xs.reverse)
  private def unpackVec(x: UInt, n: Int, w: Int): Vec[UInt] = {
    val out = Wire(Vec(n, UInt(w.W)))
    for (i <- 0 until n) out(i) := x((i + 1) * w - 1, i * w)
    out
  }

  private def split64(x: UInt, w: Int): Vec[UInt] = unpackVec(x, 64, w)
  private def split16(x: UInt, w: Int): Vec[UInt] = unpackVec(x, 16, w)
  private def split32(x: UInt, w: Int): Vec[UInt] = unpackVec(x, 32, w)

  val aLaneRam = Seq.fill(16)(Module(new SpRam(64 * 24, 2)))
  val bLaneRam = Seq.fill(16)(Module(new SpRam(64 * 8, 2)))
  val evalARam = Module(new SpRam(16 * A_EVAL_W, 343))
  val evalBRam = Module(new SpRam(16 * B_EVAL_W, 343))
  val coreRam = Seq.fill(7)(Module(new SpRam(16 * 36, 49)))
  val w1Ram = Seq.fill(7)(Module(new SpRam(32 * 33, 7 * 2)))
  val w0Ram = Seq.fill(7)(Module(new SpRam(32 * 27, 8)))
  val outRam = Module(new SpRam(32 * 24, 32))

  private def ramDefaults(ram: SpRam, width: Int): Unit = {
    ram.io.clk := clock
    ram.io.en := false.B
    ram.io.we := false.B
    ram.io.addr := 0.U
    ram.io.din := 0.U(width.W)
  }
  aLaneRam.foreach(ramDefaults(_, 64 * 24))
  bLaneRam.foreach(ramDefaults(_, 64 * 8))
  ramDefaults(evalARam, 16 * A_EVAL_W)
  ramDefaults(evalBRam, 16 * B_EVAL_W)
  coreRam.foreach(ramDefaults(_, 16 * 36))
  w1Ram.foreach(ramDefaults(_, 32 * 33))
  w0Ram.foreach(ramDefaults(_, 32 * 27))
  ramDefaults(outRam, 32 * 24)

  val sIdle :: sLoadInput :: sEvalPreRead :: sEvalCapture :: sEval :: sCore :: sInter1 :: sInter2 :: sInter3 :: sReadOutput :: sDone :: Nil = Enum(11)
  val state = RegInit(sIdle)

  val loadCount = RegInit(0.U(2.W))       // reaches 1 packed 16-lane input load
  val evalCount = RegInit(0.U(9.W))       // reaches 343 job-wide evaluations
  val coreCount = RegInit(0.U(9.W))       // reaches 343 job-wide core writes
  val inter1Count = RegInit(0.U(8.W))     // reaches 49 * (2 steps + 1 correction) = 147
  val inter2Count = RegInit(0.U(7.W))     // reaches 7 * (8 steps + 1 correction) = 63
  val inter3Count = RegInit(0.U(6.W))     // reaches 32 steps + 1 correction = 33
  val outputReadCount = RegInit(0.U(6.W)) // reaches 32 packed output reads

  val outReg = Reg(Vec(1024, UInt(24.W)))
  io.c := outReg
  io.valid_out := state === sDone

  val laneAWord = Reg(Vec(16, UInt((64 * 24).W)))
  val laneBWord = Reg(Vec(16, UInt((64 * 8).W)))

  val evalPt0 = RegInit(0.U(3.W))
  val evalPt1 = RegInit(0.U(3.W))
  val evalPt2 = RegInit(0.U(3.W))
  val evalJobIdx = evalPt0 * 49.U + evalPt1 * 7.U + evalPt2 // jobIdx = pt0 * 49 + pt1 * 7 + pt2.

  val evalA = Seq.fill(16)(Module(new Eval64Point(24, A_EVAL_W)))
  val evalB = Seq.fill(16)(Module(new Eval64Point(8, B_EVAL_W)))
  val evalAVec = Wire(Vec(16, UInt(A_EVAL_W.W)))
  val evalBVec = Wire(Vec(16, UInt(B_EVAL_W.W)))
  for (lane <- 0 until 16) {
    evalA(lane).io.in := split64(laneAWord(lane), 24)
    evalB(lane).io.in := split64(laneBWord(lane), 8)
    evalA(lane).io.pt0 := evalPt0
    evalA(lane).io.pt1 := evalPt1
    evalA(lane).io.pt2 := evalPt2
    evalB(lane).io.pt0 := evalPt0
    evalB(lane).io.pt1 := evalPt1
    evalB(lane).io.pt2 := evalPt2
    evalAVec(lane) := evalA(lane).io.out
    evalBVec(lane) := evalB(lane).io.out
  }

  val core = Module(new Core16TC4)
  core.io.valid_in := false.B
  core.io.avec := split16(evalARam.io.dout, A_EVAL_W)
  core.io.bvec := split16(evalBRam.io.dout, B_EVAL_W)

  val coreReqIdx = RegInit(0.U(9.W))
  val coreReadValid = RegInit(false.B)
  val coreFeedJob = RegInit(0.U(9.W))
  val coreWriteJob = RegInit(0.U(9.W))
  val coreWritePt2 = coreWriteJob % 7.U
  val coreWritePage = coreWriteJob / 7.U // page = pt0 * 7 + pt1, bank = pt2.

  val iPt0 = RegInit(0.U(3.W))
  val iPt1 = RegInit(0.U(3.W))
  val iStep = RegInit(0.U(5.W))
  val interSub = RegInit(0.U(2.W))

  val i1Pr0 = RegInit(0.U(30.W)); val i1Pr1 = RegInit(0.U(30.W)); val i1Pr2 = RegInit(0.U(30.W))
  val i2Pr0 = RegInit(0.U(27.W)); val i2Pr1 = RegInit(0.U(27.W)); val i2Pr2 = RegInit(0.U(27.W))
  val i3Pr0 = RegInit(0.U(24.W)); val i3Pr1 = RegInit(0.U(24.W)); val i3Pr2 = RegInit(0.U(24.W))
  val firstW1 = Reg(Vec(32, UInt(33.W)))
  val firstW0 = Reg(Vec(32, UInt(27.W)))
  val firstOut = Reg(Vec(32, UInt(24.W)))

  val interp1 = Module(new Interp8ColsStepTC4(pidx = 1, inW = 36, outW = 33))
  val interp2 = Module(new Interp8ColsStepTC4(pidx = 2, inW = 33, outW = 27))
  val interp3 = Module(new Interp8ColsStepTC4(pidx = 3, inW = 27, outW = 24))

  val inter1In = Wire(Vec(7 * 8, UInt(36.W)))
  val inter2In = Wire(Vec(7 * 8, UInt(33.W)))
  val inter3In = Wire(Vec(7 * 8, UInt(27.W)))
  for (pt <- 0 until 7) {
    val coreWord = split16(coreRam(pt).io.dout, 36)
    val w1Word = split32(w1Ram(pt).io.dout, 33)
    val w0Word = split32(w0Ram(pt).io.dout, 27)
    val coreOffset = Cat(iStep(0), 0.U(3.W))
    val inBlockOffset = Cat(iStep(1, 0), 0.U(3.W))
    for (col <- 0 until 8) {
      inter1In(pt * 8 + col) := coreWord(coreOffset + col.U)
      inter2In(pt * 8 + col) := w1Word(inBlockOffset + col.U)
      inter3In(pt * 8 + col) := w0Word(inBlockOffset + col.U)
    }
  }
  interp1.io.in := inter1In
  interp1.io.pr0 := i1Pr0; interp1.io.pr1 := i1Pr1; interp1.io.pr2 := i1Pr2
  interp2.io.in := inter2In
  interp2.io.pr0 := i2Pr0; interp2.io.pr1 := i2Pr1; interp2.io.pr2 := i2Pr2
  interp3.io.in := inter3In
  interp3.io.pr0 := i3Pr0; interp3.io.pr1 := i3Pr1; interp3.io.pr2 := i3Pr2

  val correctedW1Vec = Wire(Vec(32, UInt(33.W)))
  correctedW1Vec := firstW1
  correctedW1Vec(0) := mask(firstW1(0) - i1Pr2, 33)
  correctedW1Vec(1) := mask(firstW1(1) - i1Pr1, 33)
  correctedW1Vec(2) := mask(firstW1(2) - i1Pr0, 33)
  val correctedW1Word = packVec(correctedW1Vec)

  val correctedW0Vec = Wire(Vec(32, UInt(27.W)))
  correctedW0Vec := firstW0
  correctedW0Vec(0) := mask(firstW0(0) - i2Pr2, 27)
  correctedW0Vec(1) := mask(firstW0(1) - i2Pr1, 27)
  correctedW0Vec(2) := mask(firstW0(2) - i2Pr0, 27)
  val correctedW0Word = packVec(correctedW0Vec)

  val correctedOutVec = Wire(Vec(32, UInt(24.W)))
  correctedOutVec := firstOut
  correctedOutVec(0) := mask(firstOut(0) - i3Pr2, 24)
  correctedOutVec(1) := mask(firstOut(1) - i3Pr1, 24)
  correctedOutVec(2) := mask(firstOut(2) - i3Pr0, 24)
  val correctedOutWord = packVec(correctedOutVec)

  when(state === sIdle) {
    when(io.valid_in) {
      state := sLoadInput
      loadCount := 0.U
      evalCount := 0.U
      coreCount := 0.U
      inter1Count := 0.U
      inter2Count := 0.U
      inter3Count := 0.U
      outputReadCount := 0.U
    }
  }.elsewhen(state === sLoadInput) {
    for (lane <- 0 until 16) {
      aLaneRam(lane).io.en := true.B
      aLaneRam(lane).io.we := true.B
      aLaneRam(lane).io.addr := 0.U
      aLaneRam(lane).io.din := packVec((0 until 64).map(i => io.a(lane * 64 + i)))
      bLaneRam(lane).io.en := true.B
      bLaneRam(lane).io.we := true.B
      bLaneRam(lane).io.addr := 0.U
      bLaneRam(lane).io.din := packVec((0 until 64).map(i => io.b(lane * 64 + i)))
    }
    loadCount := 1.U
    state := sEvalPreRead
  }.elsewhen(state === sEvalPreRead) {
    for (lane <- 0 until 16) {
      aLaneRam(lane).io.en := true.B
      aLaneRam(lane).io.addr := 0.U
      bLaneRam(lane).io.en := true.B
      bLaneRam(lane).io.addr := 0.U
    }
    state := sEvalCapture
  }.elsewhen(state === sEvalCapture) {
    for (lane <- 0 until 16) {
      laneAWord(lane) := aLaneRam(lane).io.dout
      laneBWord(lane) := bLaneRam(lane).io.dout
    }
    evalPt0 := 0.U
    evalPt1 := 0.U
    evalPt2 := 0.U
    evalCount := 0.U
    state := sEval
  }.elsewhen(state === sEval) {
    evalARam.io.en := true.B
    evalARam.io.we := true.B
    evalARam.io.addr := evalJobIdx
    evalARam.io.din := packVec(evalAVec)
    evalBRam.io.en := true.B
    evalBRam.io.we := true.B
    evalBRam.io.addr := evalJobIdx
    evalBRam.io.din := packVec(evalBVec)
    evalCount := evalCount + 1.U

    when(evalPt0 === 6.U && evalPt1 === 6.U && evalPt2 === 6.U) {
      state := sCore
      coreReqIdx := 0.U
      coreReadValid := false.B
      coreFeedJob := 0.U
      coreWriteJob := 0.U
      coreCount := 0.U
    }.otherwise {
      when(evalPt2 === 6.U) {
        evalPt2 := 0.U
        when(evalPt1 === 6.U) { evalPt1 := 0.U; evalPt0 := evalPt0 + 1.U }
          .otherwise { evalPt1 := evalPt1 + 1.U }
      }.otherwise { evalPt2 := evalPt2 + 1.U }
    }
  }.elsewhen(state === sCore) {
    val doCoreRead = coreReqIdx < 343.U
    when(doCoreRead) {
      evalARam.io.en := true.B
      evalARam.io.addr := coreReqIdx // evalAddr = jobIdx, one packed 16-lane job per word.
      evalBRam.io.en := true.B
      evalBRam.io.addr := coreReqIdx
      coreReqIdx := coreReqIdx + 1.U
    }

    core.io.valid_in := coreReadValid
    when(coreReadValid) {
      coreWriteJob := coreFeedJob
    }
    coreReadValid := doCoreRead
    coreFeedJob := coreReqIdx

    when(core.io.valid_out) {
      for (pt2 <- 0 until 7) {
        when(coreWritePt2 === pt2.U) {
          coreRam(pt2).io.en := true.B
          coreRam(pt2).io.we := true.B
          coreRam(pt2).io.addr := coreWritePage // coreRam(pt2)(page), page = pt0 * 7 + pt1.
          coreRam(pt2).io.din := packVec(core.io.cOut)
        }
      }
      coreCount := coreCount + 1.U
      when(coreWriteJob === 342.U) {
        state := sInter1
        iPt0 := 0.U
        iPt1 := 0.U
        iStep := 0.U
        interSub := 0.U
        i1Pr0 := 0.U; i1Pr1 := 0.U; i1Pr2 := 0.U
      }
    }
  }.elsewhen(state === sInter1) {
    val page = iPt0 * 7.U + iPt1 // page = pt0 * 7 + pt1.
    when(interSub === 0.U) {
      for (pt2 <- 0 until 7) {
        coreRam(pt2).io.en := true.B
        coreRam(pt2).io.addr := page // Read coreRam(pt2)(page) from all seven pt2 banks in parallel.
      }
      interSub := 1.U
    }.elsewhen(interSub === 1.U) {
      val outWord = packVec(interp1.io.out)
      when(iStep === 0.U) { firstW1 := interp1.io.out }
      for (pt1 <- 0 until 7) {
        when(iPt1 === pt1.U) {
          w1Ram(pt1).io.en := true.B
          w1Ram(pt1).io.we := true.B
          w1Ram(pt1).io.addr := iPt0 * 2.U + iStep // w1Ram(pt1)(pt0 * 2 + block32).
          w1Ram(pt1).io.din := outWord
        }
      }
      inter1Count := inter1Count + 1.U
      i1Pr0 := interp1.io.nr0; i1Pr1 := interp1.io.nr1; i1Pr2 := interp1.io.nr2
      when(iStep === 0.U) {
        for (pt2 <- 0 until 7) {
          coreRam(pt2).io.en := true.B
          coreRam(pt2).io.addr := page
        }
        iStep := 1.U
      }.otherwise {
        interSub := 2.U
      }
    }.otherwise {
      for (pt1 <- 0 until 7) {
        when(iPt1 === pt1.U) {
          w1Ram(pt1).io.en := true.B
          w1Ram(pt1).io.we := true.B
          w1Ram(pt1).io.addr := iPt0 * 2.U // Correction rewrite for W1 block32 0.
          w1Ram(pt1).io.din := correctedW1Word
        }
      }
      inter1Count := inter1Count + 1.U
      i1Pr0 := 0.U; i1Pr1 := 0.U; i1Pr2 := 0.U
      iStep := 0.U
      interSub := 0.U
      when(iPt0 === 6.U && iPt1 === 6.U) {
        state := sInter2
        iPt0 := 0.U
        iStep := 0.U
        i2Pr0 := 0.U; i2Pr1 := 0.U; i2Pr2 := 0.U
      }.elsewhen(iPt1 === 6.U) {
        iPt1 := 0.U
        iPt0 := iPt0 + 1.U
      }.otherwise { iPt1 := iPt1 + 1.U }
    }
  }.elsewhen(state === sInter2) {
    when(interSub === 0.U) {
      val sourceBlock32 = iStep >> 2
      for (pt1 <- 0 until 7) {
        w1Ram(pt1).io.en := true.B
        w1Ram(pt1).io.addr := iPt0 * 2.U + sourceBlock32 // w1Ram(pt1)(pt0 * 2 + input block32).
      }
      interSub := 1.U
    }.elsewhen(interSub === 1.U) {
      val outWord = packVec(interp2.io.out)
      when(iStep === 0.U) { firstW0 := interp2.io.out }
      for (pt0 <- 0 until 7) {
        when(iPt0 === pt0.U) {
          w0Ram(pt0).io.en := true.B
          w0Ram(pt0).io.we := true.B
          w0Ram(pt0).io.addr := iStep // w0Ram(pt0)(block32).
          w0Ram(pt0).io.din := outWord
        }
      }
      inter2Count := inter2Count + 1.U
      i2Pr0 := interp2.io.nr0; i2Pr1 := interp2.io.nr1; i2Pr2 := interp2.io.nr2
      when(iStep === 7.U) {
        interSub := 2.U
      }.otherwise {
        val nextBlock32 = (iStep + 1.U) >> 2
        for (pt1 <- 0 until 7) {
          w1Ram(pt1).io.en := true.B
          w1Ram(pt1).io.addr := iPt0 * 2.U + nextBlock32
        }
        iStep := iStep + 1.U
      }
    }.otherwise {
      for (pt0 <- 0 until 7) {
        when(iPt0 === pt0.U) {
          w0Ram(pt0).io.en := true.B
          w0Ram(pt0).io.we := true.B
          w0Ram(pt0).io.addr := 0.U // Correction rewrite for W0 block32 0.
          w0Ram(pt0).io.din := correctedW0Word
        }
      }
      inter2Count := inter2Count + 1.U
      i2Pr0 := 0.U; i2Pr1 := 0.U; i2Pr2 := 0.U
      iStep := 0.U
      interSub := 0.U
      when(iPt0 === 6.U) {
        state := sInter3
        iStep := 0.U
        i3Pr0 := 0.U; i3Pr1 := 0.U; i3Pr2 := 0.U
      }.otherwise { iPt0 := iPt0 + 1.U }
    }
  }.elsewhen(state === sInter3) {
    when(interSub === 0.U) {
      val sourceBlock32 = iStep >> 2
      for (pt0 <- 0 until 7) {
        w0Ram(pt0).io.en := true.B
        w0Ram(pt0).io.addr := sourceBlock32 // w0Ram(pt0)(input block32).
      }
      interSub := 1.U
    }.elsewhen(interSub === 1.U) {
      val outWord = packVec(interp3.io.out)
      when(iStep === 0.U) { firstOut := interp3.io.out }
      outRam.io.en := true.B
      outRam.io.we := true.B
      outRam.io.addr := iStep // outRam(block32), each word holds 32 final coefficients.
      outRam.io.din := outWord
      inter3Count := inter3Count + 1.U
      i3Pr0 := interp3.io.nr0; i3Pr1 := interp3.io.nr1; i3Pr2 := interp3.io.nr2
      when(iStep === 31.U) {
        interSub := 2.U
      }.otherwise {
        val nextBlock32 = (iStep + 1.U) >> 2
        for (pt0 <- 0 until 7) {
          w0Ram(pt0).io.en := true.B
          w0Ram(pt0).io.addr := nextBlock32
        }
        iStep := iStep + 1.U
      }
    }.otherwise {
      outRam.io.en := true.B
      outRam.io.we := true.B
      outRam.io.addr := 0.U // Correction rewrite for output block32 0.
      outRam.io.din := correctedOutWord
      inter3Count := inter3Count + 1.U
      outputReadCount := 0.U
      interSub := 0.U
      state := sReadOutput
    }
  }.elsewhen(state === sReadOutput) {
    when(interSub === 0.U) {
      outRam.io.en := true.B
      outRam.io.addr := outputReadCount(4, 0) // outAddr = packed block32.
      interSub := 1.U
    }.otherwise {
      val word = split32(outRam.io.dout, 24)
      for (i <- 0 until 32) outReg(outputReadCount * 32.U + i.U) := word(i)
      when(outputReadCount === 31.U) {
        outputReadCount := outputReadCount + 1.U
        state := sDone
        interSub := 0.U
      }.otherwise {
        outputReadCount := outputReadCount + 1.U
        outRam.io.en := true.B
        outRam.io.addr := (outputReadCount + 1.U)(4, 0)
      }
    }
  }.elsewhen(state === sDone) {
    state := sIdle
  }
}

class ToomCook1024Clean extends ToomCook43Clean
class ToomCook43 extends ToomCook43Clean
