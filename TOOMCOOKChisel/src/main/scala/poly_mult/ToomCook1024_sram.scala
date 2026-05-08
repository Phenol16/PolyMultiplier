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
  private val EVAL_DEPTH = 343 * 16
  private val CORE_DEPTH = 343 * 16
  private val W1_DEPTH = 7 * 7 * 64
  private val W0_DEPTH = 7 * 256

  val aInRam = Module(new SpRam(24, 1024))
  val bInRam = Module(new SpRam(8, 1024))
  val evalARam = Module(new SpRam(A_EVAL_W, EVAL_DEPTH))
  val evalBRam = Module(new SpRam(B_EVAL_W, EVAL_DEPTH))
  val coreRam = Module(new SpRam(36, CORE_DEPTH))
  val w1Ram = Module(new SpRam(33, W1_DEPTH))
  val w0Ram = Module(new SpRam(27, W0_DEPTH))
  val outRam = Module(new SpRam(24, 1024))

  private def ramDefaults(ram: SpRam, width: Int): Unit = {
    ram.io.clk := clock
    ram.io.en := false.B
    ram.io.we := false.B
    ram.io.addr := 0.U
    ram.io.din := 0.U(width.W)
  }
  ramDefaults(aInRam, 24)
  ramDefaults(bInRam, 8)
  ramDefaults(evalARam, A_EVAL_W)
  ramDefaults(evalBRam, B_EVAL_W)
  ramDefaults(coreRam, 36)
  ramDefaults(w1Ram, 33)
  ramDefaults(w0Ram, 27)
  ramDefaults(outRam, 24)

  val sIdle :: sLoadInput :: sEval :: sCore :: sInter1 :: sInter2 :: sInter3 :: sReadOutput :: sDone :: Nil = Enum(9)
  val state = RegInit(sIdle)

  val loadCount = RegInit(0.U(11.W))       // reaches 1024
  val evalCount = RegInit(0.U(13.W))       // reaches 343 * 16 lane evaluations
  val coreCount = RegInit(0.U(9.W))        // reaches 343 jobs
  val inter1Count = RegInit(0.U(8.W))      // reaches 49 * 4 groups
  val inter2Count = RegInit(0.U(7.W))      // reaches 7 * 16 groups
  val inter3Count = RegInit(0.U(7.W))      // reaches 64 groups
  val outputReadCount = RegInit(0.U(11.W)) // reaches 1024

  // Final parallel IO mirror only; all stored output data is first written to outRam.
  val outReg = Reg(Vec(1024, UInt(24.W)))
  io.c := outReg
  io.valid_out := state === sDone

  val evalPt0 = RegInit(0.U(3.W))
  val evalPt1 = RegInit(0.U(3.W))
  val evalPt2 = RegInit(0.U(3.W))
  val evalLane = RegInit(0.U(4.W))
  val evalCoeff = RegInit(0.U(6.W))
  val evalSub = RegInit(0.U(2.W))
  val localA64 = Reg(Vec(64, UInt(24.W)))
  val localB64 = Reg(Vec(64, UInt(8.W)))
  val evalA = Module(new Eval64Point(24, A_EVAL_W))
  val evalB = Module(new Eval64Point(8, B_EVAL_W))
  evalA.io.in := localA64
  evalB.io.in := localB64
  evalA.io.pt0 := evalPt0
  evalA.io.pt1 := evalPt1
  evalA.io.pt2 := evalPt2
  evalB.io.pt0 := evalPt0
  evalB.io.pt1 := evalPt1
  evalB.io.pt2 := evalPt2

  val coreSub = RegInit(0.U(3.W))
  val coreCoeff = RegInit(0.U(4.W))
  val coreA16 = Reg(Vec(16, UInt(A_EVAL_W.W)))
  val coreB16 = Reg(Vec(16, UInt(B_EVAL_W.W)))
  val coreOut16 = Reg(Vec(16, UInt(36.W)))
  val core = Module(new Core16TC4)
  core.io.valid_in := false.B
  core.io.avec := coreA16
  core.io.bvec := coreB16

  val interSub = RegInit(0.U(2.W))
  val interRead = RegInit(0.U(5.W))
  val interWrite = RegInit(0.U(4.W))
  val iPt0 = RegInit(0.U(3.W))
  val iPt1 = RegInit(0.U(3.W))
  val iColBase = RegInit(0.U(8.W))
  val interIn36 = Reg(Vec(28, UInt(36.W)))
  val interIn33 = Reg(Vec(28, UInt(33.W)))
  val interIn27 = Reg(Vec(28, UInt(27.W)))
  val interp1 = Module(new Interp4ColsTC4(pidx = 1, inW = 36, outW = 33))
  val interp2 = Module(new Interp4ColsTC4(pidx = 2, inW = 33, outW = 27))
  val interp3 = Module(new Interp4ColsTC4(pidx = 3, inW = 27, outW = 24))
  interp1.io.in := interIn36
  interp2.io.in := interIn33
  interp3.io.in := interIn27

  val outReadSub = RegInit(0.U(1.W))

  // jobIdx = pt0 * 49 + pt1 * 7 + pt2.
  val evalJobIdx = evalPt0 * 49.U + evalPt1 * 7.U + evalPt2
  // evalAddr = jobIdx * 16 + lane.
  val evalAddr = (evalJobIdx << 4) + evalLane
  // coreAddr = jobIdx * 16 + coeff.
  val coreAddr = (coreCount << 4) + coreCoeff
  // w1Addr = (pt0 * 7 + pt1) * 64 + col.
  val w1BaseAddr = ((iPt0 * 7.U + iPt1) << 6)
  // w0Addr = pt0 * 256 + col.
  val w0BaseAddr = (iPt0 << 8)

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
    aInRam.io.en := true.B
    aInRam.io.we := true.B
    val loadIdx = loadCount(9, 0)
    aInRam.io.addr := loadIdx
    aInRam.io.din := io.a(loadIdx)
    bInRam.io.en := true.B
    bInRam.io.we := true.B
    bInRam.io.addr := loadIdx
    bInRam.io.din := io.b(loadIdx)
    when(loadCount === 1023.U) {
      loadCount := loadCount + 1.U
      state := sEval
      evalPt0 := 0.U
      evalPt1 := 0.U
      evalPt2 := 0.U
      evalLane := 0.U
      evalCoeff := 0.U
      evalSub := 0.U
    }.otherwise {
      loadCount := loadCount + 1.U
    }
  }.elsewhen(state === sEval) {
    val inputAddr = (evalLane << 6) + evalCoeff // input coefficient address = lane * 64 + coefficient.
    when(evalSub === 0.U) {
      aInRam.io.en := true.B
      aInRam.io.addr := inputAddr
      bInRam.io.en := true.B
      bInRam.io.addr := inputAddr
      evalSub := 1.U
    }.elsewhen(evalSub === 1.U) {
      localA64(evalCoeff) := aInRam.io.dout
      localB64(evalCoeff) := bInRam.io.dout
      when(evalCoeff === 63.U) {
        evalSub := 2.U
      }.otherwise {
        evalCoeff := evalCoeff + 1.U
        evalSub := 0.U
      }
    }.otherwise {
      evalARam.io.en := true.B
      evalARam.io.we := true.B
      evalARam.io.addr := evalAddr
      evalARam.io.din := evalA.io.out
      evalBRam.io.en := true.B
      evalBRam.io.we := true.B
      evalBRam.io.addr := evalAddr
      evalBRam.io.din := evalB.io.out
      evalCount := evalCount + 1.U
      evalCoeff := 0.U
      evalSub := 0.U
      when(evalPt0 === 6.U && evalPt1 === 6.U && evalPt2 === 6.U && evalLane === 15.U) {
        state := sCore
        coreCount := 0.U
        coreCoeff := 0.U
        coreSub := 0.U
      }.otherwise {
        when(evalLane === 15.U) {
          evalLane := 0.U
          when(evalPt2 === 6.U) {
            evalPt2 := 0.U
            when(evalPt1 === 6.U) {
              evalPt1 := 0.U
              evalPt0 := evalPt0 + 1.U
            }.otherwise { evalPt1 := evalPt1 + 1.U }
          }.otherwise { evalPt2 := evalPt2 + 1.U }
        }.otherwise { evalLane := evalLane + 1.U }
      }
    }
  }.elsewhen(state === sCore) {
    when(coreSub === 0.U) {
      evalARam.io.en := true.B
      evalARam.io.addr := coreAddr
      evalBRam.io.en := true.B
      evalBRam.io.addr := coreAddr
      coreSub := 1.U
    }.elsewhen(coreSub === 1.U) {
      coreA16(coreCoeff) := evalARam.io.dout
      coreB16(coreCoeff) := evalBRam.io.dout
      when(coreCoeff === 15.U) {
        coreSub := 2.U
      }.otherwise {
        coreCoeff := coreCoeff + 1.U
        coreSub := 0.U
      }
    }.elsewhen(coreSub === 2.U) {
      core.io.valid_in := true.B
      coreSub := 3.U
    }.elsewhen(coreSub === 3.U) {
      when(core.io.valid_out) {
        coreOut16 := core.io.cOut
        coreCoeff := 0.U
        coreSub := 4.U
      }
    }.otherwise {
      coreRam.io.en := true.B
      coreRam.io.we := true.B
      coreRam.io.addr := (coreCount << 4) + coreCoeff // coreAddr = jobIdx * 16 + coeff.
      coreRam.io.din := coreOut16(coreCoeff)
      when(coreCoeff === 15.U) {
        coreCount := coreCount + 1.U
        coreCoeff := 0.U
        coreSub := 0.U
        when(coreCount === 342.U) {
          state := sInter1
          iPt0 := 0.U
          iPt1 := 0.U
          iColBase := 0.U
          interRead := 0.U
          interWrite := 0.U
          interSub := 0.U
        }
      }.otherwise { coreCoeff := coreCoeff + 1.U }
    }
  }.elsewhen(state === sInter1) {
    val pt2 = interRead >> 2
    val col = iColBase + interRead(1, 0)
    val readJob = iPt0 * 49.U + iPt1 * 7.U + pt2 // jobIdx = pt0 * 49 + pt1 * 7 + pt2.
    when(interSub === 0.U) {
      coreRam.io.en := true.B
      coreRam.io.addr := (readJob << 4) + col // coreAddr = jobIdx * 16 + col.
      interSub := 1.U
    }.elsewhen(interSub === 1.U) {
      interIn36(interRead) := coreRam.io.dout
      when(interRead === 27.U) { interSub := 2.U; interWrite := 0.U }
      .otherwise { interRead := interRead + 1.U; interSub := 0.U }
    }.otherwise {
      w1Ram.io.en := true.B
      w1Ram.io.we := true.B
      w1Ram.io.addr := w1BaseAddr + (iColBase << 2) + interWrite // w1Addr = (pt0 * 7 + pt1) * 64 + colBase * 4 + outIdx.
      w1Ram.io.din := interp1.io.out(interWrite)
      when(interWrite === 15.U) {
        inter1Count := inter1Count + 1.U
        interRead := 0.U
        interWrite := 0.U
        interSub := 0.U
        when(iPt0 === 6.U && iPt1 === 6.U && iColBase === 12.U) {
          state := sInter2
          iPt0 := 0.U
          iPt1 := 0.U
          iColBase := 0.U
        }.elsewhen(iColBase === 12.U) {
          iColBase := 0.U
          when(iPt1 === 6.U) { iPt1 := 0.U; iPt0 := iPt0 + 1.U }
          .otherwise { iPt1 := iPt1 + 1.U }
        }.otherwise { iColBase := iColBase + 4.U }
      }.otherwise { interWrite := interWrite + 1.U }
    }
  }.elsewhen(state === sInter2) {
    val pt1 = interRead >> 2
    val col = iColBase + interRead(1, 0)
    when(interSub === 0.U) {
      w1Ram.io.en := true.B
      w1Ram.io.addr := ((iPt0 * 7.U + pt1) << 6) + col // w1Addr = (pt0 * 7 + pt1) * 64 + col.
      interSub := 1.U
    }.elsewhen(interSub === 1.U) {
      interIn33(interRead) := w1Ram.io.dout
      when(interRead === 27.U) { interSub := 2.U; interWrite := 0.U }
      .otherwise { interRead := interRead + 1.U; interSub := 0.U }
    }.otherwise {
      w0Ram.io.en := true.B
      w0Ram.io.we := true.B
      w0Ram.io.addr := w0BaseAddr + (iColBase << 2) + interWrite // w0Addr = pt0 * 256 + colBase * 4 + outIdx.
      w0Ram.io.din := interp2.io.out(interWrite)
      when(interWrite === 15.U) {
        inter2Count := inter2Count + 1.U
        interRead := 0.U
        interWrite := 0.U
        interSub := 0.U
        when(iPt0 === 6.U && iColBase === 60.U) {
          state := sInter3
          iPt0 := 0.U
          iColBase := 0.U
        }.elsewhen(iColBase === 60.U) {
          iColBase := 0.U
          iPt0 := iPt0 + 1.U
        }.otherwise { iColBase := iColBase + 4.U }
      }.otherwise { interWrite := interWrite + 1.U }
    }
  }.elsewhen(state === sInter3) {
    val pt0 = interRead >> 2
    val col = iColBase + interRead(1, 0)
    when(interSub === 0.U) {
      w0Ram.io.en := true.B
      w0Ram.io.addr := (pt0 << 8) + col // w0Addr = pt0 * 256 + col.
      interSub := 1.U
    }.elsewhen(interSub === 1.U) {
      interIn27(interRead) := w0Ram.io.dout
      when(interRead === 27.U) { interSub := 2.U; interWrite := 0.U }
      .otherwise { interRead := interRead + 1.U; interSub := 0.U }
    }.otherwise {
      outRam.io.en := true.B
      outRam.io.we := true.B
      outRam.io.addr := (iColBase << 2) + interWrite // outAddr = colBase * 4 + outIdx.
      outRam.io.din := interp3.io.out(interWrite)
      when(interWrite === 15.U) {
        inter3Count := inter3Count + 1.U
        interRead := 0.U
        interWrite := 0.U
        interSub := 0.U
        when(iColBase === 252.U) {
          state := sReadOutput
          outputReadCount := 0.U
          outReadSub := 0.U
        }.otherwise { iColBase := iColBase + 4.U }
      }.otherwise { interWrite := interWrite + 1.U }
    }
  }.elsewhen(state === sReadOutput) {
    when(outReadSub === 0.U) {
      outRam.io.en := true.B
      val outIdx = outputReadCount(9, 0)
      outRam.io.addr := outIdx // outAddr = col.
      outReadSub := 1.U
    }.otherwise {
      val outIdx = outputReadCount(9, 0)
      outReg(outIdx) := outRam.io.dout
      when(outputReadCount === 1023.U) {
        outputReadCount := outputReadCount + 1.U
        state := sDone
      }.otherwise {
        outputReadCount := outputReadCount + 1.U
        outReadSub := 0.U
      }
    }
  }.elsewhen(state === sDone) {
    state := sIdle
  }
}

class ToomCook1024Clean extends ToomCook43Clean
class ToomCook43 extends ToomCook43Clean
