package poly_mult_sram

import chisel3._
import chisel3.util._
import core._

object TC4EvalWidth {
  val A_EVAL_W = 39
  val B_EVAL_W = 29
}

object InterpParamTable {
  case class Param(mk: Int, mk2: Int, mk3: Int, inv3: BigInt, inv9: BigInt, inv15: BigInt)

  val params = Seq(
    Param(36, 33, 34, BigInt("AAAAAAAAB", 16), BigInt("238E38E39", 16), BigInt("2EEEEEEEF", 16)),
    Param(33, 30, 31, BigInt("2AAAAAAB", 16), BigInt("38E38E39", 16), BigInt("6EEEEEEF", 16)),
    Param(30, 27, 28, BigInt("2AAAAAB", 16), BigInt("8E38E39", 16), BigInt("EEEEEEF", 16)),
    Param(27, 24, 25, BigInt("AAAAAB", 16), BigInt("E38E39", 16), BigInt("EEEEEEF", 16))
  )
}

/* 4 个输入
  → core.Eval 算出 7 个点
  → 根据 pt 选择其中 1 个点
  → 输出 */
class EvalPoint(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r = Input(Vec(4, UInt(inW.W)))
    val pt = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  val layer = Module(new Eval(inWidth = inW, outWidth = outW))
  layer.io.in := io.r
  io.out := MuxLookup(io.pt, 0.U(outW.W))((0 until 7).map(i => i.U -> layer.io.out(i))) //根据 io.pt 的值，从 layer.io.out(0..6) 中选择一个作为 io.out
}

/* 输入 64 个系数
根据 pt0, pt1, pt2 三个 evaluation point
输出一个最终估值结果 */
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
      val eval = Module(new EvalPoint(inW, outW))
      for (inner <- 0 until 4) eval.io.r(inner) := io.in(outer * 16 + middle * 4 + inner)//a[outer][middle][inner]
      eval.io.pt := io.pt0
      mid(outer * 4 + middle) := eval.io.out
    }
  }

  val high = Wire(Vec(4, UInt(outW.W)))
  for (outer <- 0 until 4) {
    val eval = Module(new EvalPoint(outW, outW))
    for (middle <- 0 until 4) eval.io.r(middle) := mid(outer * 4 + middle)
    eval.io.pt := io.pt1
    high(outer) := eval.io.out
  }

  val eval = Module(new EvalPoint(outW, outW))
  eval.io.r := high
  eval.io.pt := io.pt2
  io.out := eval.io.out
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

class ToomCook1024IO extends Bundle {
  val start = Input(Bool())
  val busy = Output(Bool())
  val done = Output(Bool())
  val a_we = Input(Bool())
  val a_addr = Input(UInt(4.W))
  val a_din = Input(UInt((64 * 24).W))
  val b_we = Input(Bool())
  val b_addr = Input(UInt(4.W))
  val b_din = Input(UInt((64 * 8).W))
  val c_re = Input(Bool())
  val c_addr = Input(UInt(4.W))
  val c_dout = Output(UInt((64 * 24).W))
  val c_valid = Output(Bool())
}

class ToomCook1024 extends Module {
  val io = IO(new ToomCook1024IO)

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
  private def pack16(xs: Seq[UInt]): UInt = {
    require(xs.length == 16)
    packVec(xs)
  }

  private def evalBank(job: UInt): UInt = job(0)
  private def evalAddr(job: UInt): UInt = job >> 1
  private def pageBuf(page: UInt): UInt = page(0)
  private def pageAddr(page: UInt): UInt = page >> 1

  private val ColsPerBank = 16
  private val CoreGroups = 1
  private val GroupsPerBlock = 4

  val inARam = Module(new Sram1536x16)
  val inBRam = Module(new Sram512x16)

  val evalARam = Seq.fill(2)(Module(new Sram624x172))
  val evalBRam = Seq.fill(2)(Module(new Sram464x172))

  val coreRam = Seq.fill(2, 7, CoreGroups)(Module(new Sram576x25))
  val w1Buf = Reg(Vec(2, Vec(7, Vec(GroupsPerBlock, UInt((ColsPerBank * 33).W)))))
  // w0Buf replaces w0Ram in the fixed 16-column design.
  // The original w0Ram depth is only 4, which is too shallow for the available
  // SRAM macros. Using Reg avoids wasting 32-depth SRAM macro rows and also
  // removes the one-cycle SRAM read prefetch in Interp3.
  val w0Buf = Reg(Vec(7, Vec(GroupsPerBlock, Vec(4, UInt((ColsPerBank * 27).W)))))

  val outRam = Module(new Sram1536x16)

  private def ramDefaults(ram: FixedSram): Unit = {
    ram.io.clk := clock
    ram.io.en := false.B
    ram.io.we := false.B
    ram.io.addr := 0.U
    ram.io.din := 0.U.asTypeOf(ram.io.din)
  }
  ramDefaults(inARam)
  ramDefaults(inBRam)
  evalARam.foreach(ramDefaults)
  evalBRam.foreach(ramDefaults)
  for (buf <- 0 until 2; pt2 <- 0 until 7; group <- 0 until CoreGroups) {
    ramDefaults(coreRam(buf)(pt2)(group))
  }
  ramDefaults(outRam)

  val doneReg = RegInit(false.B)
  io.done := doneReg

  val laneAWord = Reg(Vec(16, UInt((64 * 24).W)))
  val laneBWord = Reg(Vec(16, UInt((64 * 8).W)))
  val loadActive = RegInit(false.B)
  val loadLane = RegInit(0.U(4.W))
  val loadPhase = RegInit(false.B)
  

  val evalActive = RegInit(false.B)
  val evalProduced = RegInit(0.U(9.W))
  val evalPt0 = RegInit(0.U(3.W))
  val evalPt1 = RegInit(0.U(3.W))
  val evalPt2 = RegInit(0.U(3.W))
  // Flat eval job counter. It advances in lockstep with evalPt0/evalPt1/evalPt2
  // to avoid hardware multipliers for pt0*49 + pt1*7 + pt2.
  val evalJobIdx = RegInit(0.U(9.W))

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

  val core = Module(new core16(t = 0, k = 2, sign = 1, aWidth = A_EVAL_W, bWidth = B_EVAL_W, cWidth = 36))
  core.io.valid_in := false.B
  val coreAWordReg = Reg(UInt((16 * A_EVAL_W).W))
  val coreBWordReg = Reg(UInt((16 * B_EVAL_W).W))
  val coreWordValid = RegInit(false.B)
  core.io.a := split16(coreAWordReg, A_EVAL_W)
  core.io.b := split16(coreBWordReg, B_EVAL_W)

  val coreActive = RegInit(false.B)
  val coreReqIdx = RegInit(0.U(9.W))
  val coreReadValid = RegInit(false.B)
  val coreFeedJob = RegInit(0.U(9.W))
  // Core output write pointer. It replaces the old flat coreOutJob counter.
  val coreWrPage = RegInit(0.U(6.W))
  val coreWrPt2 = RegInit(0.U(3.W))
  val corePageReady = RegInit(VecInit(Seq.fill(49)(false.B)))

  val i1Active = RegInit(false.B)
  val i1Page = RegInit(0.U(6.W))
  val i1Pt0 = RegInit(0.U(3.W))
  val i1Pt1 = RegInit(0.U(3.W))
  val i1Sub = RegInit(0.U(2.W))
  val i1Pr0 = RegInit(0.U(30.W)); val i1Pr1 = RegInit(0.U(30.W)); val i1Pr2 = RegInit(0.U(30.W))
  val firstW1 = Reg(Vec(64, UInt(33.W)))
  val w1PageReady = RegInit(VecInit(Seq.fill(49)(false.B)))
  val w1GroupReady = RegInit(VecInit(Seq.fill(7)(false.B)))

  val i2Active = RegInit(false.B)
  val i2Pt0 = RegInit(0.U(3.W))
  val i2Step = RegInit(0.U(3.W))
  val i2Sub = RegInit(0.U(2.W))
  val i2Pr0 = RegInit(0.U(27.W)); val i2Pr1 = RegInit(0.U(27.W)); val i2Pr2 = RegInit(0.U(27.W))
  val firstW0 = Reg(Vec(64, UInt(27.W)))
  val w0BlockReady = RegInit(VecInit(Seq.fill(7)(false.B)))

  val i3Active = RegInit(false.B)
  val i3Step = RegInit(0.U(4.W))
  val i3Sub = RegInit(0.U(2.W))
  val i3Pr0 = RegInit(0.U(24.W)); val i3Pr1 = RegInit(0.U(24.W)); val i3Pr2 = RegInit(0.U(24.W))
  val firstOut = Reg(Vec(64, UInt(24.W)))

  val interp1 = Module(new Interp16ColsStep(pidx = 1, inW = 36, outW = 33))
  val interp2 = Module(new Interp16ColsStep(pidx = 2, inW = 33, outW = 27))
  val interp3 = Module(new Interp16ColsStep(pidx = 3, inW = 27, outW = 24))

  val inter1In = Wire(Vec(7 * 16, UInt(36.W)))
  val inter2In = Wire(Vec(7 * 16, UInt(33.W)))
  val inter3In = Wire(Vec(7 * 16, UInt(27.W)))
  val i2Group = i2Step(1, 0)
  val i3Group = i3Step(1, 0)
  val i3Addr = i3Step(3, 2)

  for (pt <- 0 until 7) {
    val i1Packed16 = Mux(pageBuf(i1Page).asBool, coreRam(1)(pt)(0).io.dout, coreRam(0)(pt)(0).io.dout)
    val i2Packed16 = Mux(i2Pt0(0), w1Buf(1)(pt)(i2Group), w1Buf(0)(pt)(i2Group))
    val i3Packed16 = w0Buf(pt)(i3Group)(i3Addr)
    val i1Word16 = split16(i1Packed16, 36)
    val i2Word16 = split16(i2Packed16, 33)
    val i3Word16 = split16(i3Packed16, 27)
    for (col <- 0 until 16) {
      inter1In(pt * 16 + col) := i1Word16(col)
      inter2In(pt * 16 + col) := i2Word16(col)
      inter3In(pt * 16 + col) := i3Word16(col)
    }
  }
  interp1.io.in := inter1In
  interp1.io.pr0 := i1Pr0; interp1.io.pr1 := i1Pr1; interp1.io.pr2 := i1Pr2
  interp2.io.in := inter2In
  interp2.io.pr0 := i2Pr0; interp2.io.pr1 := i2Pr1; interp2.io.pr2 := i2Pr2
  interp3.io.in := inter3In
  interp3.io.pr0 := i3Pr0; interp3.io.pr1 := i3Pr1; interp3.io.pr2 := i3Pr2

  // firstW1/firstW0/firstOut hold raw block0. After the final 16-column step of
  // a full stride, i1Pr/i2Pr/i3Pr hold final carry; the correction stage then
  // overwrites block0 coeff 0/1/2 using those final carries.
  val correctedW1Vec = Wire(Vec(64, UInt(33.W)))
  correctedW1Vec := firstW1
  correctedW1Vec(0) := ParaMath.mask(firstW1(0) - i1Pr2, 33)
  correctedW1Vec(1) := ParaMath.mask(firstW1(1) - i1Pr1, 33)
  correctedW1Vec(2) := ParaMath.mask(firstW1(2) - i1Pr0, 33)
  val correctedW1WordG0 = pack16((0 until ColsPerBank).map(i => correctedW1Vec(i)))

  val correctedW0Vec = Wire(Vec(64, UInt(27.W)))
  correctedW0Vec := firstW0
  correctedW0Vec(0) := ParaMath.mask(firstW0(0) - i2Pr2, 27)
  correctedW0Vec(1) := ParaMath.mask(firstW0(1) - i2Pr1, 27)
  correctedW0Vec(2) := ParaMath.mask(firstW0(2) - i2Pr0, 27)
  val correctedW0WordG0 = pack16((0 until ColsPerBank).map(i => correctedW0Vec(i)))

  val correctedOutVec = Wire(Vec(64, UInt(24.W)))
  correctedOutVec := firstOut
  correctedOutVec(0) := ParaMath.mask(firstOut(0) - i3Pr2, 24)
  correctedOutVec(1) := ParaMath.mask(firstOut(1) - i3Pr1, 24)
  correctedOutVec(2) := ParaMath.mask(firstOut(2) - i3Pr0, 24)
  val correctedOutWord = packVec(correctedOutVec)

  val computing = loadActive || evalActive || coreActive || i1Active || i2Active || i3Active
  io.busy := computing
  val idle = !io.busy

  when(idle && io.a_we) {
    inARam.io.en := true.B
    inARam.io.we := true.B
    inARam.io.addr := io.a_addr
    inARam.io.din := io.a_din
  }
  when(idle && io.b_we) {
    inBRam.io.en := true.B
    inBRam.io.we := true.B
    inBRam.io.addr := io.b_addr
    inBRam.io.din := io.b_din
  }

  when(io.start && idle) {
    doneReg := false.B
    loadActive := true.B
    loadLane := 0.U
    loadPhase := false.B
    evalActive := false.B
    evalProduced := 0.U
    evalPt0 := 0.U; evalPt1 := 0.U; evalPt2 := 0.U
    evalJobIdx := 0.U
    coreActive := false.B
    coreReqIdx := 0.U
    coreReadValid := false.B
    coreWordValid := false.B
    coreFeedJob := 0.U
    coreWrPage := 0.U
    coreWrPt2 := 0.U
    i1Active := false.B; i1Page := 0.U; i1Sub := 0.U
    i1Pt0 := 0.U; i1Pt1 := 0.U
    i2Active := false.B; i2Pt0 := 0.U; i2Step := 0.U; i2Sub := 0.U
    i3Active := false.B; i3Step := 0.U; i3Sub := 0.U
    i1Pr0 := 0.U; i1Pr1 := 0.U; i1Pr2 := 0.U
    i2Pr0 := 0.U; i2Pr1 := 0.U; i2Pr2 := 0.U
    i3Pr0 := 0.U; i3Pr1 := 0.U; i3Pr2 := 0.U
    corePageReady := VecInit(Seq.fill(49)(false.B))
    w1PageReady := VecInit(Seq.fill(49)(false.B))
    w1GroupReady := VecInit(Seq.fill(7)(false.B))
    w0BlockReady := VecInit(Seq.fill(7)(false.B))
  }

  when(loadActive) {
    when(!loadPhase) {
      inARam.io.en := true.B; inARam.io.addr := loadLane
      inBRam.io.en := true.B; inBRam.io.addr := loadLane
      loadPhase := true.B
    }.otherwise {
      laneAWord(loadLane) := inARam.io.dout
      laneBWord(loadLane) := inBRam.io.dout
      when(loadLane === 15.U) {
        loadActive := false.B
        evalActive := true.B
        coreActive := true.B
      }.otherwise {
        loadLane := loadLane + 1.U
        loadPhase := false.B
      }
    }
  }

  when(evalActive) {
    for (bank <- 0 until 2) {
      when(evalBank(evalJobIdx) === bank.U) {
        evalARam(bank).io.en := true.B
        evalARam(bank).io.we := true.B
        evalARam(bank).io.addr := evalAddr(evalJobIdx)
        evalARam(bank).io.din := packVec(evalAVec)
        evalBRam(bank).io.en := true.B
        evalBRam(bank).io.we := true.B
        evalBRam(bank).io.addr := evalAddr(evalJobIdx)
        evalBRam(bank).io.din := packVec(evalBVec)
      }
    }
    val isLastEvalJob = evalPt0 === 6.U && evalPt1 === 6.U && evalPt2 === 6.U
    evalProduced := evalProduced + 1.U
    when(isLastEvalJob) {
      evalActive := false.B
    }.otherwise {
      evalJobIdx := evalJobIdx + 1.U
      when(evalPt2 === 6.U) {
        evalPt2 := 0.U
        when(evalPt1 === 6.U) { evalPt1 := 0.U; evalPt0 := evalPt0 + 1.U }
          .otherwise { evalPt1 := evalPt1 + 1.U }
      }.otherwise { evalPt2 := evalPt2 + 1.U }
    }
  }

  // CoreController: read job k after eval has written it. While Eval writes job
  // k+1 in bank (k+1)%2, Core reads job k from bank k%2, so no single-port SRAM
  // read/write conflict occurs.
  when(coreActive) {
    val doCoreRead = coreReqIdx < evalProduced
    when(doCoreRead) {
      for (bank <- 0 until 2) {
        when(evalBank(coreReqIdx) === bank.U) {
          evalARam(bank).io.en := true.B
          evalARam(bank).io.addr := evalAddr(coreReqIdx)
          evalBRam(bank).io.en := true.B
          evalBRam(bank).io.addr := evalAddr(coreReqIdx)
        }
      }
      coreReqIdx := coreReqIdx + 1.U
    }

    core.io.valid_in := coreWordValid
    when(coreReadValid) {
      for (bank <- 0 until 2) {
        when(evalBank(coreFeedJob) === bank.U) {
          coreAWordReg := evalARam(bank).io.dout
          coreBWordReg := evalBRam(bank).io.dout
        }
      }
    }
    coreWordValid := coreReadValid
    coreReadValid := doCoreRead
    coreFeedJob := coreReqIdx
    when(core.io.valid_out) {
      for (buf <- 0 until 2; pt2 <- 0 until 7) {
        when(pageBuf(coreWrPage) === buf.U && coreWrPt2 === pt2.U) {
          coreRam(buf)(pt2)(0).io.en := true.B
          coreRam(buf)(pt2)(0).io.we := true.B
          coreRam(buf)(pt2)(0).io.addr := pageAddr(coreWrPage)
          coreRam(buf)(pt2)(0).io.din := pack16((0 until 16).map(col => core.io.c(col)))
        }
      }
      when(coreWrPt2 === 6.U) { corePageReady(coreWrPage) := true.B }
      when(coreWrPage === 48.U && coreWrPt2 === 6.U) {
        coreActive := false.B
      }.otherwise {
        when(coreWrPt2 === 6.U) {
          coreWrPt2 := 0.U
          coreWrPage := coreWrPage + 1.U
        }.otherwise {
          coreWrPt2 := coreWrPt2 + 1.U
        }
      }
    }
  }

  // Inter1Controller: consume each core page as soon as its seven pt2 banks are
  // ready. Core writes page p+1 while Inter1 reads page p; page%2 ping-ponging
  // avoids coreRam single-port conflicts.
  when(!i1Active && corePageReady(i1Page) && !w1PageReady(i1Page)) {
    val rdBuf = pageBuf(i1Page)
    val rdAddr = pageAddr(i1Page)
    for (pt2 <- 0 until 7) {
      for (buf <- 0 until 2) {
        when(rdBuf === buf.U) {
          coreRam(buf)(pt2)(0).io.en := true.B
          coreRam(buf)(pt2)(0).io.addr := rdAddr
        }
      }
    }
    i1Active := true.B
    i1Sub := 1.U
    i1Pr0 := 0.U; i1Pr1 := 0.U; i1Pr2 := 0.U
  }.elsewhen(i1Active) {
    val pt0 = i1Pt0
    val pt1 = i1Pt1
    val wrBuf = pt0(0)
    when(i1Sub === 1.U) {
      firstW1 := interp1.io.out
      for (buf <- 0 until 2; bank <- 0 until 7; group <- 0 until GroupsPerBlock) {
        val groupWord = pack16((0 until ColsPerBank).map(col => interp1.io.out(group * ColsPerBank + col)))
        when(wrBuf === buf.U && pt1 === bank.U) {
          w1Buf(buf)(bank)(group) := groupWord
        }
      }
      i1Pr0 := interp1.io.nr0; i1Pr1 := interp1.io.nr1; i1Pr2 := interp1.io.nr2
      i1Sub := 2.U
    }.otherwise {
      for (buf <- 0 until 2; bank <- 0 until 7) {
        when(wrBuf === buf.U && pt1 === bank.U) {
          w1Buf(buf)(bank)(0) := correctedW1WordG0
        }
      }
      w1PageReady(i1Page) := true.B
      when(pt1 === 6.U) { w1GroupReady(pt0) := true.B }
      i1Active := false.B
      i1Pr0 := 0.U; i1Pr1 := 0.U; i1Pr2 := 0.U
      when(i1Page =/= 48.U) {
        i1Page := i1Page + 1.U
        when(i1Pt1 === 6.U) {
          i1Pt1 := 0.U
          i1Pt0 := i1Pt0 + 1.U
        }.otherwise {
          i1Pt1 := i1Pt1 + 1.U
        }
      }
    }
  }

  // Inter2Controller: consume a complete pt0 group after all seven W1 pt1 banks
  // are ready. Inter1 writes pt0+1 into the opposite pt0%2 W1 buffer while
  // Inter2 reads pt0.
  when(!i2Active && w1GroupReady(i2Pt0) && !w0BlockReady(i2Pt0)) {
    i2Active := true.B
    i2Sub := 1.U
    i2Step := 0.U
    i2Pr0 := 0.U; i2Pr1 := 0.U; i2Pr2 := 0.U
  }.elsewhen(i2Active) {
    when(i2Sub === 1.U) {
      val w0WriteAddr = i2Step(1, 0)
      when(i2Step === 0.U) { firstW0 := interp2.io.out }
      for (pt0 <- 0 until 7; group <- 0 until GroupsPerBlock) {
        when(i2Pt0 === pt0.U) {
          w0Buf(pt0)(group)(w0WriteAddr) := pack16((0 until ColsPerBank).map(col => interp2.io.out(group * ColsPerBank + col)))
        }
      }
      i2Pr0 := interp2.io.nr0; i2Pr1 := interp2.io.nr1; i2Pr2 := interp2.io.nr2
      when(i2Step === 3.U) {
        i2Sub := 2.U
      }.otherwise {
        i2Step := i2Step + 1.U
      }
    }.otherwise {
      for (pt0 <- 0 until 7) {
        when(i2Pt0 === pt0.U) {
          w0Buf(pt0)(0)(0) := correctedW0WordG0
        }
      }
      w0BlockReady(i2Pt0) := true.B
      i2Active := false.B
      i2Pr0 := 0.U; i2Pr1 := 0.U; i2Pr2 := 0.U
      when(i2Pt0 =/= 6.U) { i2Pt0 := i2Pt0 + 1.U }
    }
  }

  // Inter3Controller: wait until all W0 pt0 banks are complete, then produce the
  // final 64-wide output blocks.
  when(!i3Active && !doneReg && w0BlockReady.asUInt.andR) {
    i3Active := true.B
    i3Sub := 1.U
    i3Step := 0.U
    i3Pr0 := 0.U; i3Pr1 := 0.U; i3Pr2 := 0.U
  }.elsewhen(i3Active) {
    when(i3Sub === 1.U) {
      val outWord = packVec(interp3.io.out)
      when(i3Step === 0.U) { firstOut := interp3.io.out }
      outRam.io.en := true.B
      outRam.io.we := true.B
      outRam.io.addr := i3Step
      outRam.io.din := outWord
      i3Pr0 := interp3.io.nr0; i3Pr1 := interp3.io.nr1; i3Pr2 := interp3.io.nr2
      when(i3Step === 15.U) {
        i3Sub := 2.U
      }.otherwise {
        i3Step := i3Step + 1.U
      }
    }.otherwise {
      outRam.io.en := true.B
      outRam.io.we := true.B
      outRam.io.addr := 0.U
      outRam.io.din := correctedOutWord
      doneReg := true.B
      i3Active := false.B
    }
  }

  when(!io.busy && doneReg && io.c_re) {
    outRam.io.en := true.B
    outRam.io.we := false.B
    outRam.io.addr := io.c_addr
  }
  io.c_dout := outRam.io.dout
  io.c_valid := RegNext(io.c_re && doneReg, false.B)
}