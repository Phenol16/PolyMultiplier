package poly_mult_sram

import chisel3._
import chisel3.util._
import core._

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


class TC4EvalPoint(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r = Input(Vec(4, UInt(inW.W)))
    val pt = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  val layer = Module(new Eval(inWidth = inW, outWidth = outW))
  layer.io.in := io.r
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

class InterpStepCoreTC4(pidx: Int, inW: Int) extends Module {
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
  val r5b = ParaMath.mask(ParaMath.mask((r5a + r1b) >> 1, mk) * p.inv18.U(42.W), mk3)
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
    val core = Module(new InterpStepCoreTC4(pidx, inW))
    for (pt <- 0 until 7) core.io.pIn(pt) := io.in(pt * 8 + col)
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

  io.nr0 := carry0(8)
  io.nr1 := carry1(8)
  io.nr2 := carry2(8)
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
  val start = Input(Bool())
  val busy = Output(Bool())
  val done = Output(Bool())
  val a_we = Input(Bool())
  val a_addr = Input(UInt(5.W))
  val a_din = Input(UInt((32 * 24).W))
  val b_we = Input(Bool())
  val b_addr = Input(UInt(5.W))
  val b_din = Input(UInt((32 * 8).W))
  val c_re = Input(Bool())
  val c_addr = Input(UInt(5.W))
  val c_dout = Output(UInt((32 * 24).W))
  val c_valid = Output(Bool())
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

  private def evalBank(job: UInt): UInt = job(0)
  private def evalAddr(job: UInt): UInt = job >> 1
  private def pageOf(job: UInt): UInt = job / 7.U
  private def pt2Of(job: UInt): UInt = job - pageOf(job) * 7.U
  private def pageBuf(page: UInt): UInt = page(0)
  private def pageAddr(page: UInt): UInt = page >> 1
  private def pt0OfPage(page: UInt): UInt = page / 7.U
  private def pt1OfPage(page: UInt): UInt = page - pt0OfPage(page) * 7.U

  val inARam = Module(new SpRam(32 * 24, 32))
  val inBRam = Module(new SpRam(32 * 8, 32))
  val evalARam = Seq.fill(2)(Module(new SpRam(16 * A_EVAL_W, 172)))
  val evalBRam = Seq.fill(2)(Module(new SpRam(16 * B_EVAL_W, 172)))
  val coreRam = Seq.fill(2, 7)(Module(new SpRam(16 * 36, 25)))
  val w1Ram = Seq.fill(2, 7)(Module(new SpRam(32 * 33, 2)))
  val w0Ram = Seq.fill(7)(Module(new SpRam(32 * 27, 8)))
  val outRam = Module(new SpRam(32 * 24, 32))

  private def ramDefaults(ram: SpRam, width: Int): Unit = {
    ram.io.clk := clock
    ram.io.en := false.B
    ram.io.we := false.B
    ram.io.addr := 0.U
    ram.io.din := 0.U(width.W)
  }
  ramDefaults(inARam, 32 * 24)
  ramDefaults(inBRam, 32 * 8)
  evalARam.foreach(ramDefaults(_, 16 * A_EVAL_W))
  evalBRam.foreach(ramDefaults(_, 16 * B_EVAL_W))
  for (buf <- 0 until 2; pt2 <- 0 until 7) ramDefaults(coreRam(buf)(pt2), 16 * 36)
  for (buf <- 0 until 2; pt1 <- 0 until 7) ramDefaults(w1Ram(buf)(pt1), 32 * 33)
  w0Ram.foreach(ramDefaults(_, 32 * 27))
  ramDefaults(outRam, 32 * 24)

  val doneReg = RegInit(false.B)
  io.done := doneReg

  val laneAWord = Reg(Vec(16, UInt((64 * 24).W)))
  val laneBWord = Reg(Vec(16, UInt((64 * 8).W)))
  val loadActive = RegInit(false.B)
  val loadLane = RegInit(0.U(4.W))
  val loadPhase = RegInit(0.U(2.W))
  val loadA0 = Reg(UInt((32 * 24).W))
  val loadB0 = Reg(UInt((32 * 8).W))

  val evalActive = RegInit(false.B)
  val evalProduced = RegInit(0.U(9.W))
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
  val coreOutJob = RegInit(0.U(9.W))
  val corePageReady = RegInit(VecInit(Seq.fill(49)(false.B)))

  val i1Active = RegInit(false.B)
  val i1Page = RegInit(0.U(6.W))
  val i1Step = RegInit(0.U(1.W))
  val i1Sub = RegInit(0.U(2.W))
  val i1Pr0 = RegInit(0.U(30.W)); val i1Pr1 = RegInit(0.U(30.W)); val i1Pr2 = RegInit(0.U(30.W))
  val firstW1 = Reg(Vec(32, UInt(33.W)))
  val w1PageReady = RegInit(VecInit(Seq.fill(49)(false.B)))
  val w1GroupReady = RegInit(VecInit(Seq.fill(7)(false.B)))

  val i2Active = RegInit(false.B)
  val i2Pt0 = RegInit(0.U(3.W))
  val i2Step = RegInit(0.U(3.W))
  val i2Sub = RegInit(0.U(2.W))
  val i2Pr0 = RegInit(0.U(27.W)); val i2Pr1 = RegInit(0.U(27.W)); val i2Pr2 = RegInit(0.U(27.W))
  val firstW0 = Reg(Vec(32, UInt(27.W)))
  val w0BlockReady = RegInit(VecInit(Seq.fill(7)(false.B)))

  val i3Active = RegInit(false.B)
  val i3Step = RegInit(0.U(5.W))
  val i3Sub = RegInit(0.U(2.W))
  val i3Pr0 = RegInit(0.U(24.W)); val i3Pr1 = RegInit(0.U(24.W)); val i3Pr2 = RegInit(0.U(24.W))
  val firstOut = Reg(Vec(32, UInt(24.W)))

  val interp1 = Module(new Interp8ColsStepTC4(pidx = 1, inW = 36, outW = 33))
  val interp2 = Module(new Interp8ColsStepTC4(pidx = 2, inW = 33, outW = 27))
  val interp3 = Module(new Interp8ColsStepTC4(pidx = 3, inW = 27, outW = 24))

  val inter1In = Wire(Vec(7 * 8, UInt(36.W)))
  val inter2In = Wire(Vec(7 * 8, UInt(33.W)))
  val inter3In = Wire(Vec(7 * 8, UInt(27.W)))
  val i1CoreOffset = Cat(i1Step, 0.U(3.W))
  val i2BlockOffset = Cat(i2Step(1, 0), 0.U(3.W))
  val i3BlockOffset = Cat(i3Step(1, 0), 0.U(3.W))
  for (pt <- 0 until 7) {
    val i1Packed = Mux(pageBuf(i1Page).asBool, coreRam(1)(pt).io.dout, coreRam(0)(pt).io.dout)
    val i2Packed = Mux(i2Pt0(0), w1Ram(1)(pt).io.dout, w1Ram(0)(pt).io.dout)
    val i1Word = split16(i1Packed, 36)
    val i2Word = split32(i2Packed, 33)
    val i3Word = split32(w0Ram(pt).io.dout, 27)
    for (col <- 0 until 8) {
      inter1In(pt * 8 + col) := i1Word(i1CoreOffset + col.U)
      inter2In(pt * 8 + col) := i2Word(i2BlockOffset + col.U)
      inter3In(pt * 8 + col) := i3Word(i3BlockOffset + col.U)
    }
  }
  interp1.io.in := inter1In
  interp1.io.pr0 := i1Pr0; interp1.io.pr1 := i1Pr1; interp1.io.pr2 := i1Pr2
  interp2.io.in := inter2In
  interp2.io.pr0 := i2Pr0; interp2.io.pr1 := i2Pr1; interp2.io.pr2 := i2Pr2
  interp3.io.in := inter3In
  interp3.io.pr0 := i3Pr0; interp3.io.pr1 := i3Pr1; interp3.io.pr2 := i3Pr2

  // firstW1/firstW0/firstOut hold raw block0. After the final 8-column step of
  // a full stride, i1Pr/i2Pr/i3Pr hold final carry; the correction stage then
  // overwrites block0 coeff 0/1/2 using those final carries.
  val correctedW1Vec = Wire(Vec(32, UInt(33.W)))
  correctedW1Vec := firstW1
  correctedW1Vec(0) := ParaMath.mask(firstW1(0) - i1Pr2, 33)
  correctedW1Vec(1) := ParaMath.mask(firstW1(1) - i1Pr1, 33)
  correctedW1Vec(2) := ParaMath.mask(firstW1(2) - i1Pr0, 33)
  val correctedW1Word = packVec(correctedW1Vec)

  val correctedW0Vec = Wire(Vec(32, UInt(27.W)))
  correctedW0Vec := firstW0
  correctedW0Vec(0) := ParaMath.mask(firstW0(0) - i2Pr2, 27)
  correctedW0Vec(1) := ParaMath.mask(firstW0(1) - i2Pr1, 27)
  correctedW0Vec(2) := ParaMath.mask(firstW0(2) - i2Pr0, 27)
  val correctedW0Word = packVec(correctedW0Vec)

  val correctedOutVec = Wire(Vec(32, UInt(24.W)))
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
    loadPhase := 0.U
    evalActive := false.B
    evalProduced := 0.U
    evalPt0 := 0.U; evalPt1 := 0.U; evalPt2 := 0.U
    coreActive := false.B
    coreReqIdx := 0.U
    coreReadValid := false.B
    coreWordValid := false.B
    coreFeedJob := 0.U
    coreOutJob := 0.U
    i1Active := false.B; i1Page := 0.U; i1Step := 0.U; i1Sub := 0.U
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
    val baseAddr = Cat(loadLane, 0.U(1.W))
    when(loadPhase === 0.U) {
      inARam.io.en := true.B; inARam.io.addr := baseAddr
      inBRam.io.en := true.B; inBRam.io.addr := baseAddr
      loadPhase := 1.U
    }.elsewhen(loadPhase === 1.U) {
      loadA0 := inARam.io.dout
      loadB0 := inBRam.io.dout
      inARam.io.en := true.B; inARam.io.addr := baseAddr + 1.U
      inBRam.io.en := true.B; inBRam.io.addr := baseAddr + 1.U
      loadPhase := 2.U
    }.otherwise {
      laneAWord(loadLane) := Cat(inARam.io.dout, loadA0)
      laneBWord(loadLane) := Cat(inBRam.io.dout, loadB0)
      when(loadLane === 15.U) {
        loadActive := false.B
        evalActive := true.B
        coreActive := true.B
      }.otherwise {
        loadLane := loadLane + 1.U
        loadPhase := 0.U
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
    evalProduced := evalProduced + 1.U
    when(evalPt0 === 6.U && evalPt1 === 6.U && evalPt2 === 6.U) {
      evalActive := false.B
    }.otherwise {
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
      val wrPage = pageOf(coreOutJob)
      val wrPt2 = pt2Of(coreOutJob)
      for (buf <- 0 until 2; pt2 <- 0 until 7) {
        when(pageBuf(wrPage) === buf.U && wrPt2 === pt2.U) {
          coreRam(buf)(pt2).io.en := true.B
          coreRam(buf)(pt2).io.we := true.B
          coreRam(buf)(pt2).io.addr := pageAddr(wrPage) // coreRam(page%2)(pt2)(page/2).
          coreRam(buf)(pt2).io.din := packVec(core.io.c)
        }
      }
      when(wrPt2 === 6.U) { corePageReady(wrPage) := true.B }
      when(coreOutJob === 342.U) {
        coreActive := false.B
      }.otherwise {
        coreOutJob := coreOutJob + 1.U
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
          coreRam(buf)(pt2).io.en := true.B
          coreRam(buf)(pt2).io.addr := rdAddr
        }
      }
    }
    i1Active := true.B
    i1Sub := 1.U
    i1Step := 0.U
    i1Pr0 := 0.U; i1Pr1 := 0.U; i1Pr2 := 0.U
  }.elsewhen(i1Active) {
    val pt0 = pt0OfPage(i1Page)
    val pt1 = pt1OfPage(i1Page)
    val wrBuf = pt0(0)
    when(i1Sub === 1.U) {
      val outWord = packVec(interp1.io.out)
      when(i1Step === 0.U) { firstW1 := interp1.io.out }
      for (buf <- 0 until 2; bank <- 0 until 7) {
        when(wrBuf === buf.U && pt1 === bank.U) {
          w1Ram(buf)(bank).io.en := true.B
          w1Ram(buf)(bank).io.we := true.B
          w1Ram(buf)(bank).io.addr := i1Step // w1Ram(pt0%2)(pt1)(block32).
          w1Ram(buf)(bank).io.din := outWord
        }
      }
      i1Pr0 := interp1.io.nr0; i1Pr1 := interp1.io.nr1; i1Pr2 := interp1.io.nr2
      when(i1Step === 0.U) {
        val rdBuf = pageBuf(i1Page)
        val rdAddr = pageAddr(i1Page)
        for (pt2 <- 0 until 7; buf <- 0 until 2) {
          when(rdBuf === buf.U) {
            coreRam(buf)(pt2).io.en := true.B
            coreRam(buf)(pt2).io.addr := rdAddr
          }
        }
        i1Step := 1.U
      }.otherwise { i1Sub := 2.U }
    }.otherwise {
      for (buf <- 0 until 2; bank <- 0 until 7) {
        when(wrBuf === buf.U && pt1 === bank.U) {
          w1Ram(buf)(bank).io.en := true.B
          w1Ram(buf)(bank).io.we := true.B
          w1Ram(buf)(bank).io.addr := 0.U // Correction rewrite for W1 block32 0.
          w1Ram(buf)(bank).io.din := correctedW1Word
        }
      }
      w1PageReady(i1Page) := true.B
      when(pt1 === 6.U) { w1GroupReady(pt0) := true.B }
      i1Active := false.B
      i1Pr0 := 0.U; i1Pr1 := 0.U; i1Pr2 := 0.U
      when(i1Page =/= 48.U) { i1Page := i1Page + 1.U }
    }
  }

  // Inter2Controller: consume a complete pt0 group after all seven W1 pt1 banks
  // are ready. Inter1 writes pt0+1 into the opposite pt0%2 W1 buffer while
  // Inter2 reads pt0.
  when(!i2Active && w1GroupReady(i2Pt0) && !w0BlockReady(i2Pt0)) {
    val rdBuf = i2Pt0(0)
    for (pt1 <- 0 until 7; buf <- 0 until 2) {
      when(rdBuf === buf.U) {
        w1Ram(buf)(pt1).io.en := true.B
        w1Ram(buf)(pt1).io.addr := 0.U
      }
    }
    i2Active := true.B
    i2Sub := 1.U
    i2Step := 0.U
    i2Pr0 := 0.U; i2Pr1 := 0.U; i2Pr2 := 0.U
  }.elsewhen(i2Active) {
    when(i2Sub === 1.U) {
      val outWord = packVec(interp2.io.out)
      when(i2Step === 0.U) { firstW0 := interp2.io.out }
      for (pt0 <- 0 until 7) {
        when(i2Pt0 === pt0.U) {
          w0Ram(pt0).io.en := true.B
          w0Ram(pt0).io.we := true.B
          w0Ram(pt0).io.addr := i2Step // w0Ram(pt0)(block32).
          w0Ram(pt0).io.din := outWord
        }
      }
      i2Pr0 := interp2.io.nr0; i2Pr1 := interp2.io.nr1; i2Pr2 := interp2.io.nr2
      when(i2Step === 7.U) {
        i2Sub := 2.U
      }.otherwise {
        val nextBlock32 = (i2Step + 1.U) >> 2
        val rdBuf = i2Pt0(0)
        for (pt1 <- 0 until 7; buf <- 0 until 2) {
          when(rdBuf === buf.U) {
            w1Ram(buf)(pt1).io.en := true.B
            w1Ram(buf)(pt1).io.addr := nextBlock32
          }
        }
        i2Step := i2Step + 1.U
      }
    }.otherwise {
      for (pt0 <- 0 until 7) {
        when(i2Pt0 === pt0.U) {
          w0Ram(pt0).io.en := true.B
          w0Ram(pt0).io.we := true.B
          w0Ram(pt0).io.addr := 0.U // Correction rewrite for W0 block32 0.
          w0Ram(pt0).io.din := correctedW0Word
        }
      }
      w0BlockReady(i2Pt0) := true.B
      i2Active := false.B
      i2Pr0 := 0.U; i2Pr1 := 0.U; i2Pr2 := 0.U
      when(i2Pt0 =/= 6.U) { i2Pt0 := i2Pt0 + 1.U }
    }
  }

  // Inter3Controller: wait until all W0 pt0 banks are complete, then produce the
  // final 32-wide output blocks. Each Inter3 write also updates outReg directly,
  // so there is no output readback state on the critical path.
  when(!i3Active && !doneReg && w0BlockReady.asUInt.andR) {
    for (pt0 <- 0 until 7) {
      w0Ram(pt0).io.en := true.B
      w0Ram(pt0).io.addr := 0.U
    }
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
      when(i3Step === 31.U) {
        i3Sub := 2.U
      }.otherwise {
        val nextBlock32 = (i3Step + 1.U) >> 2
        for (pt0 <- 0 until 7) {
          w0Ram(pt0).io.en := true.B
          w0Ram(pt0).io.addr := nextBlock32
        }
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

class ToomCook1024Clean extends ToomCook43Clean
class ToomCook43 extends ToomCook43Clean
