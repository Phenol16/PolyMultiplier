package poly_mult_sram

import chisel3._
import chisel3.util._

// Legacy monolithic SRAM Toom-Cook top kept as a bit-for-bit reference for the
// clarity-first modular ToomCook43 implementation in ToomCook1024_sram.scala.
class ToomCook43LegacyIO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(1024, UInt(24.W)))
  val b = Input(Vec(1024, UInt(8.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(1024, UInt(24.W)))
}

class ToomCook43Legacy extends Module {
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
  val io = IO(new ToomCook43LegacyIO)

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