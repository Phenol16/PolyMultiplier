package High

import chisel3._
import chisel3.util._
import core._

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

  private val A_EVAL_W = EvalWidth.A_EVAL_W
  private val B_EVAL_W = EvalWidth.B_EVAL_W
//向量打包和拆分
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
//bank和地址计算
  private def lowBitBank(x: UInt): UInt = x(0)
  private def halfAddr(x: UInt): UInt = x >> 1
  private def evalBank(job: UInt): UInt = lowBitBank(job)
  private def evalAddr(job: UInt): UInt = halfAddr(job)
  private def pageBuf(page: UInt): UInt = lowBitBank(page)
  private def pageAddr(page: UInt): UInt = halfAddr(page)

//存储模块实例化

  val inARam = Module(new SpRam(64 * 24, 16))
  val inBRam = Module(new SpRam(64 * 8, 16))
  val outRam = Module(new SpRam(64 * 24, 16))
  val evalARam = Seq.fill(2)(Module(new SpRam(16 * A_EVAL_W, 172)))
  val evalBRam = Seq.fill(2)(Module(new SpRam(16 * B_EVAL_W, 172)))
  val coreRam = Seq.fill(2, 7)(Module(new SpRam(16 * 36, 25)))

  val laneAWord = Reg(Vec(16, UInt((64 * 24).W)))
  val laneBWord = Reg(Vec(16, UInt((64 * 8).W)))
  val w1Buf = Reg(Vec(2, Vec(7, Vec(4, UInt((16 * 33).W)))))
  val w0Buf = Reg(Vec(7, Vec(4, Vec(4, UInt((16 * 27).W)))))
// RAM 默认值设置
  private def ramDefaults(ram: SpRam): Unit = {
    ram.io.clk := clock
    ram.io.en := false.B
    ram.io.we := false.B
    ram.io.addr := 0.U.asTypeOf(ram.io.addr)
    ram.io.din := 0.U.asTypeOf(ram.io.din)
  }
  ramDefaults(inARam)
  ramDefaults(inBRam)
  evalARam.foreach(r => ramDefaults(r))
  evalBRam.foreach(r => ramDefaults(r))
  for (buf <- 0 until 2; pt2 <- 0 until 7) ramDefaults(coreRam(buf)(pt2))
  ramDefaults(outRam)

  val doneReg = RegInit(false.B)
  io.done := doneReg

  val loadActive = RegInit(false.B)
  val loadLane = RegInit(0.U(4.W))
  val loadPhase = RegInit(false.B)

  val evalActive = RegInit(false.B)
  val evalProduced = RegInit(0.U(9.W))
  val evalPt0 = RegInit(0.U(3.W))
  val evalPt1 = RegInit(0.U(3.W))
  val evalPt2 = RegInit(0.U(3.W))
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

  val evalAWord = packVec(evalAVec)
  val evalBWord = packVec(evalBVec)
  val coreOutWord = pack16((0 until 16).map(col => core.io.c(col)))
  core.io.valid_in := coreActive && coreWordValid

  val core = Module(new core16(t = 0, k = 2, sign = 1, aWidth = A_EVAL_W, bWidth = B_EVAL_W, cWidth = 36))
  val coreAWordReg = Reg(UInt((16 * A_EVAL_W).W))
  val coreBWordReg = Reg(UInt((16 * B_EVAL_W).W))
  val coreWordValid = RegInit(false.B)
  core.io.a := split16(coreAWordReg, A_EVAL_W)
  core.io.b := split16(coreBWordReg, B_EVAL_W)

  val coreActive = RegInit(false.B)
  val coreReqIdx = RegInit(0.U(9.W))
  val coreReadValid = RegInit(false.B)
  val coreFeedBank = RegInit(0.U(1.W))
  val coreWrPage = RegInit(0.U(6.W))
  val coreWrPt2 = RegInit(0.U(3.W))
  val corePagesReady = RegInit(0.U(6.W))

  val i1Active = RegInit(false.B)
  val i1Page = RegInit(0.U(6.W))
  val i1Pt0 = RegInit(0.U(3.W))
  val i1Pt1 = RegInit(0.U(3.W))
  val i1Correct = RegInit(false.B)
  val i1Pr = RegInit(VecInit(Seq.fill(3)(0.U(30.W))))
  val firstW1 = Reg(Vec(16, UInt(33.W)))
  val w1GroupsReady = RegInit(0.U(4.W))

  val i2Active = RegInit(false.B)
  val i2Pt0 = RegInit(0.U(3.W))
  val i2Step = RegInit(0.U(3.W))
  val i2Correct = RegInit(false.B)
  val i2Pr = RegInit(VecInit(Seq.fill(3)(0.U(27.W))))
  val firstW0 = Reg(Vec(16, UInt(27.W)))
  val w0BlocksReady = RegInit(0.U(4.W))

  val i3Active = RegInit(false.B)
  val i3Step = RegInit(0.U(4.W))
  val i3Correct = RegInit(false.B)
  val i3Pr = RegInit(VecInit(Seq.fill(3)(0.U(24.W))))
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
    val i1Packed16 = Mux(pageBuf(i1Page).asBool, coreRam(1)(pt).io.dout, coreRam(0)(pt).io.dout)
    val i2Packed16 = w1Buf(i2Pt0(0))(pt)(i2Group)
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
  interp1.io.pr0 := i1Pr(0); interp1.io.pr1 := i1Pr(1); interp1.io.pr2 := i1Pr(2)
  interp2.io.in := inter2In
  interp2.io.pr0 := i2Pr(0); interp2.io.pr1 := i2Pr(1); interp2.io.pr2 := i2Pr(2)
  interp3.io.in := inter3In
  interp3.io.pr0 := i3Pr(0); interp3.io.pr1 := i3Pr(1); interp3.io.pr2 := i3Pr(2)

//插值回绕
  val correctedW1WordG0 = pack16((0 until 16).map {
    case 0 => ParaMath.mask(firstW1(0) - i1Pr(2), 33)
    case 1 => ParaMath.mask(firstW1(1) - i1Pr(1), 33)
    case 2 => ParaMath.mask(firstW1(2) - i1Pr(0), 33)
    case i => firstW1(i)
  })

  val correctedW0WordG0 = pack16((0 until 16).map {
    case 0 => ParaMath.mask(firstW0(0) - i2Pr(2), 27)
    case 1 => ParaMath.mask(firstW0(1) - i2Pr(1), 27)
    case 2 => ParaMath.mask(firstW0(2) - i2Pr(0), 27)
    case i => firstW0(i)
  })

  val correctedOutVec = Wire(Vec(64, UInt(24.W)))
  correctedOutVec := firstOut
  correctedOutVec(0) := ParaMath.mask(firstOut(0) - i3Pr(2), 24)
  correctedOutVec(1) := ParaMath.mask(firstOut(1) - i3Pr(1), 24)
  correctedOutVec(2) := ParaMath.mask(firstOut(2) - i3Pr(0), 24)
  val correctedOutWord = packVec(correctedOutVec)

  val i1GroupWords = Wire(Vec(4, UInt((16 * 33).W)))
  for (group <- 0 until 4) {
    i1GroupWords(group) := pack16((0 until 16).map(col => interp1.io.out(group * 16 + col)))
  }
  val i2GroupWords = Wire(Vec(4, UInt((16 * 27).W)))
  for (group <- 0 until 4) {
    i2GroupWords(group) := pack16((0 until 16).map(col => interp2.io.out(group * 16 + col)))
  }




//忙闲状态与启动
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
    coreFeedBank := 0.U
    coreWrPage := 0.U
    coreWrPt2 := 0.U
    i1Active := false.B; i1Page := 0.U; i1Correct := false.B
    i1Pt0 := 0.U; i1Pt1 := 0.U
    i2Active := false.B; i2Pt0 := 0.U; i2Step := 0.U; i2Correct := false.B
    i3Active := false.B; i3Step := 0.U; i3Correct := false.B
    i1Pr := VecInit(Seq.fill(3)(0.U(30.W)))
    i2Pr := VecInit(Seq.fill(3)(0.U(27.W)))
    i3Pr := VecInit(Seq.fill(3)(0.U(24.W)))
    corePagesReady := 0.U
    w1GroupsReady := 0.U
    w0BlocksReady := 0.U
  }
//数据加载阶段
  when(loadActive) {
    when(!loadPhase) {
      inARam.io.en := true.B; inARam.io.addr := loadLane
      inBRam.io.en := true.B; inBRam.io.addr := loadLane
      loadPhase := true.B
    }.otherwise {
      laneAWord(loadLane) := inARam.io.dout
      laneBWord(loadLane) := inBRam.io.dout
      when(loadLane === 15.U) {//加载完成开始eval和core计算
        loadActive := false.B
        evalActive := true.B
        coreActive := true.B
      }.otherwise {
        loadLane := loadLane + 1.U
        loadPhase := false.B
      }
    }
  }

  //估值阶段
  when(evalActive) {
    for (bank <- 0 until 2) {
      when(evalBank(evalJobIdx) === bank.U) {
        evalARam(bank).io.en := true.B
        evalARam(bank).io.we := true.B
        evalARam(bank).io.addr := evalAddr(evalJobIdx)
        evalARam(bank).io.din := evalAWord
        evalBRam(bank).io.en := true.B
        evalBRam(bank).io.we := true.B
        evalBRam(bank).io.addr := evalAddr(evalJobIdx)
        evalBRam(bank).io.din := evalBWord
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
        when(evalPt1 === 6.U) { 
          evalPt1 := 0.U; 
          evalPt0 := evalPt0 + 1.U 
          }.otherwise { evalPt1 := evalPt1 + 1.U }
      }.otherwise { evalPt2 := evalPt2 + 1.U }
    }
  }

//核心计算阶段
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
      coreFeedBank := evalBank(coreReqIdx)
      coreReqIdx := coreReqIdx + 1.U
    }

    when(coreReadValid) {
      when(coreFeedBank === 0.U) {
        coreAWordReg := evalARam(0).io.dout
        coreBWordReg := evalBRam(0).io.dout
      }.otherwise {
        coreAWordReg := evalARam(1).io.dout
        coreBWordReg := evalBRam(1).io.dout
      }
    }
    coreWordValid := coreReadValid
    coreReadValid := doCoreRead
    when(core.io.valid_out) {
      for (buf <- 0 until 2; pt2 <- 0 until 7) {
        when(pageBuf(coreWrPage) === buf.U && coreWrPt2 === pt2.U) {
          coreRam(buf)(pt2).io.en := true.B
          coreRam(buf)(pt2).io.we := true.B
          coreRam(buf)(pt2).io.addr := pageAddr(coreWrPage)
          coreRam(buf)(pt2).io.din := coreOutWord
        }
      }
      when(coreWrPt2 === 6.U && corePagesReady =/= 49.U) { corePagesReady := corePagesReady + 1.U }
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

//插值1阶段
  when(!i1Active && (i1Page < corePagesReady) && (i1Page < 49.U)) {
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
    i1Correct := false.B
    i1Pr := VecInit(Seq.fill(3)(0.U(30.W)))
  }.elsewhen(i1Active) {
    val pt0 = i1Pt0
    val pt1 = i1Pt1
    val wrBuf = pt0(0)
    when(!i1Correct) {
      for (i <- 0 until 16) {
        firstW1(i) := interp1.io.out(i)
      }
      for (group <- 0 until 4) {
        w1Buf(wrBuf)(pt1)(group) := i1GroupWords(group)
      }
      i1Pr(0) := interp1.io.nr0; i1Pr(1) := interp1.io.nr1; i1Pr(2) := interp1.io.nr2
      i1Correct := true.B
    }.otherwise {
      w1Buf(wrBuf)(pt1)(0) := correctedW1WordG0
      when(pt1 === 6.U && w1GroupsReady =/= 7.U) { w1GroupsReady := w1GroupsReady + 1.U }
      i1Active := false.B
      i1Correct := false.B
      i1Pr := VecInit(Seq.fill(3)(0.U(30.W)))
      when(i1Page === 48.U) {
        i1Page := 49.U
      }.otherwise {
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
//插值2阶段
  when(!i2Active && (i2Pt0 < w1GroupsReady) && (i2Pt0 < 7.U) && (i2Pt0 === w0BlocksReady)) {
    i2Active := true.B
    i2Correct := false.B
    i2Step := 0.U
    i2Pr := VecInit(Seq.fill(3)(0.U(27.W)))
  }.elsewhen(i2Active) {
    when(!i2Correct) {
      val w0WriteAddr = i2Step(1, 0)
      when(i2Step === 0.U) {
        for (i <- 0 until 16) {
          firstW0(i) := interp2.io.out(i)
        }
      }
      for (group <- 0 until 4) {
        w0Buf(i2Pt0)(group)(w0WriteAddr) := i2GroupWords(group)
      }
      i2Pr(0) := interp2.io.nr0; i2Pr(1) := interp2.io.nr1; i2Pr(2) := interp2.io.nr2
      when(i2Step === 3.U) {
        i2Correct := true.B
      }.otherwise {
        i2Step := i2Step + 1.U
      }
    }.otherwise {
      w0Buf(i2Pt0)(0)(0) := correctedW0WordG0
      when(w0BlocksReady =/= 7.U) { w0BlocksReady := w0BlocksReady + 1.U }
      i2Active := false.B
      i2Correct := false.B
      i2Pr := VecInit(Seq.fill(3)(0.U(27.W)))
      i2Pt0 := i2Pt0 + 1.U
    }
  }

  // Inter3Controller: wait until all W0 pt0 banks are complete, then produce the
  // final 64-wide output blocks.
  when(!i3Active && !doneReg && (w0BlocksReady === 7.U)) {
    i3Active := true.B
    i3Correct := false.B
    i3Step := 0.U
    i3Pr := VecInit(Seq.fill(3)(0.U(24.W)))
  }.elsewhen(i3Active) {
    when(!i3Correct) {
      val outWord = packVec(interp3.io.out)
      when(i3Step === 0.U) { firstOut := interp3.io.out }
      outRam.io.en := true.B
      outRam.io.we := true.B
      outRam.io.addr := i3Step
      outRam.io.din := outWord
      i3Pr(0) := interp3.io.nr0; i3Pr(1) := interp3.io.nr1; i3Pr(2) := interp3.io.nr2
      when(i3Step === 15.U) {
        i3Correct := true.B
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
      i3Correct := false.B
      i3Pr := VecInit(Seq.fill(3)(0.U(24.W)))
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