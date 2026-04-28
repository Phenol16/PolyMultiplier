package poly_mult

import chisel3._
import chisel3.util._

// PolyMul1024 顶层
// 保留原本的整体流水结构：
// 输入寄存 -> Core16内部寄存 -> W2寄存 -> W1寄存 -> W0寄存 -> 输出寄存
class PolyMul1024IO extends Bundle {
  val valid_in  = Input(Bool())
  val a         = Input(Vec(1024, UInt(24.W)))
  val b         = Input(Vec(1024, UInt(8.W)))
  val valid_out = Output(Bool())
  val c         = Output(Vec(1024, UInt(24.W)))
}

class PolyMul1024 extends Module {
  val io = IO(new PolyMul1024IO)
  private val A_IN_W = PolyMulConfig.A_IN_W
  private val B_IN_W = PolyMulConfig.B_IN_W
  private val A_EVAL_W = PolyMulConfig.A_EVAL_W
  private val B_EVAL_W = PolyMulConfig.B_EVAL_W
  private val CORE_OUT_W = PolyMulConfig.CORE_OUT_W
  private val EVAL_LANES  = PolyMulConfig.EVAL_LANES
  private val EVAL_PHASES = 16 / EVAL_LANES

  object State extends ChiselEnum {
    val IDLE, RUN_CORE, WAIT_CORE,
      INTERP1_START, INTERP1_WAIT,
      INTERP2_START, INTERP2_WAIT,
      INTERP3_START, INTERP3_WAIT,
      DONE = Value
  }

  val state = RegInit(State.IDLE)

  val regA = Reg(Vec(1024, UInt(24.W)))
  val regB = Reg(Vec(1024, UInt(8.W)))

  val w2Reg = Reg(Vec(49, Vec(7 * 16, UInt(CORE_OUT_W.W))))
  val regW1 = Reg(Vec(7, Vec(7, Vec(64, UInt(33.W)))))
  val regW0 = Reg(Vec(7, Vec(256, UInt(27.W))))
  val regC  = Reg(Vec(1024, UInt(24.W)))

  val pt0Cnt   = RegInit(0.U(3.W))
  val pt1Cnt   = RegInit(0.U(3.W))
  val pt2Cnt   = RegInit(0.U(3.W))
  val groupCnt = RegInit(0.U(6.W))
  val evalPhaseCnt = RegInit(0.U(log2Ceil(EVAL_PHASES).W))
  val waitCoreCnt = RegInit(0.U(1.W))

  val interpGroupCnt = RegInit(0.U(6.W))
  val interp1BlockCnt = RegInit(0.U(3.W))
  val interp1SubCnt   = RegInit(0.U(3.W))

  val core   = Module(new Core16)
  val evalLanesA = (0 until EVAL_LANES).map(lane => Module(new EvalLaneFixed(memW = A_IN_W, outW = A_EVAL_W, laneConst = lane, evalLanes = EVAL_LANES)))
  val evalLanesB = (0 until EVAL_LANES).map(lane => Module(new EvalLaneFixed(memW = B_IN_W, outW = B_EVAL_W, laneConst = lane, evalLanes = EVAL_LANES)))
  val avecBuf = Reg(Vec(16, UInt(A_EVAL_W.W)))
  val bvecBuf = Reg(Vec(16, UInt(B_EVAL_W.W)))

  val groupPipe = Reg(UInt(6.W))
  val pt2Pipe   = Reg(UInt(3.W))
  val segVPipe  = RegInit(false.B)

  val interp16Seq  = Module(new InterpLayerSeq(stride = 16,  pidx = 1, inW = 36, outW = 33))
  val interp64Seq  = Module(new InterpLayerSeq(stride = 64,  pidx = 2, inW = 33, outW = 27))
  val interp256Seq = Module(new InterpLayerSeq(stride = 256, pidx = 3, inW = 27, outW = 24))

  io.valid_out := false.B
  io.c         := regC

  interp16Seq.io.start  := false.B
  interp64Seq.io.start  := false.B
  interp256Seq.io.start := false.B

  for (g <- 0 until 7) {
    for (k <- 0 until 256) {
      interp256Seq.io.wIn(g * 256 + k) := regW0(g)(k)
    }
  }

  val runCoreFire = state === State.RUN_CORE
  val evalLastPhase = evalPhaseCnt === (EVAL_PHASES - 1).U
  val segFire = runCoreFire && evalLastPhase

  val laneOutA = Wire(Vec(EVAL_LANES, UInt(A_EVAL_W.W)))
  val laneOutB = Wire(Vec(EVAL_LANES, UInt(B_EVAL_W.W)))
  for (lane <- 0 until EVAL_LANES) {
    evalLanesA(lane).io.in    := regA
    evalLanesA(lane).io.pt0   := pt0Cnt
    evalLanesA(lane).io.pt1   := pt1Cnt
    evalLanesA(lane).io.pt2   := pt2Cnt
    evalLanesA(lane).io.phase := evalPhaseCnt
    laneOutA(lane)            := evalLanesA(lane).io.out

    evalLanesB(lane).io.in    := regB
    evalLanesB(lane).io.pt0   := pt0Cnt
    evalLanesB(lane).io.pt1   := pt1Cnt
    evalLanesB(lane).io.pt2   := pt2Cnt
    evalLanesB(lane).io.phase := evalPhaseCnt
    laneOutB(lane)            := evalLanesB(lane).io.out
  }

  val coreAvecIn = Wire(Vec(16, UInt(A_EVAL_W.W)))
  val coreBvecIn = Wire(Vec(16, UInt(B_EVAL_W.W)))
  for (i <- 0 until 16) {
    coreAvecIn(i) := avecBuf(i)
    coreBvecIn(i) := bvecBuf(i)
  }
  for (lane <- 0 until EVAL_LANES) {
    val idx = evalPhaseCnt * EVAL_LANES.U + lane.U
    coreAvecIn(idx) := laneOutA(lane)
    coreBvecIn(idx) := laneOutB(lane)
  }

  core.io.valid_in := segFire
  core.io.avec     := coreAvecIn
  core.io.bvec     := coreBvecIn

  when(runCoreFire) {
    for (lane <- 0 until EVAL_LANES) {
      val idx = evalPhaseCnt * EVAL_LANES.U + lane.U
      avecBuf(idx) := laneOutA(lane)
      bvecBuf(idx) := laneOutB(lane)
    }
  }

  when(segFire) {
    groupPipe := groupCnt
    pt2Pipe   := pt2Cnt
    segVPipe  := true.B
  }.otherwise {
    segVPipe := false.B
  }

  when(segVPipe && core.io.valid_out) {
    switch(pt2Pipe) {
      is(0.U) {
        for (t <- 0 until 16) {
          w2Reg(groupPipe)(0 * 16 + t) := core.io.cOut(t)
        }
      }
      is(1.U) {
        for (t <- 0 until 16) {
          w2Reg(groupPipe)(1 * 16 + t) := core.io.cOut(t)
        }
      }
      is(2.U) {
        for (t <- 0 until 16) {
          w2Reg(groupPipe)(2 * 16 + t) := core.io.cOut(t)
        }
      }
      is(3.U) {
        for (t <- 0 until 16) {
          w2Reg(groupPipe)(3 * 16 + t) := core.io.cOut(t)
        }
      }
      is(4.U) {
        for (t <- 0 until 16) {
          w2Reg(groupPipe)(4 * 16 + t) := core.io.cOut(t)
        }
      }
      is(5.U) {
        for (t <- 0 until 16) {
          w2Reg(groupPipe)(5 * 16 + t) := core.io.cOut(t)
        }
      }
      is(6.U) {
        for (t <- 0 until 16) {
          w2Reg(groupPipe)(6 * 16 + t) := core.io.cOut(t)
        }
      }
    }
  }

  for (k <- 0 until 7 * 16) {
    interp16Seq.io.wIn(k) := w2Reg(interpGroupCnt)(k)
  }

  for (sub <- 0 until 7) {
    for (k <- 0 until 64) {
      interp64Seq.io.wIn(sub * 64 + k) := regW1(interpGroupCnt)(sub)(k)
    }
  }

  switch(state) {
    is(State.IDLE) {
      pt0Cnt         := 0.U
      pt1Cnt         := 0.U
      pt2Cnt         := 0.U
      groupCnt       := 0.U
      evalPhaseCnt   := 0.U
      waitCoreCnt    := 0.U
      interpGroupCnt := 0.U
      interp1BlockCnt := 0.U
      interp1SubCnt   := 0.U
      when(io.valid_in) {
        regA  := io.a
        regB  := io.b
        state := State.RUN_CORE
      }
    }

    is(State.RUN_CORE) {
      when(evalLastPhase) {
        evalPhaseCnt := 0.U
        when(pt0Cnt === 6.U && pt1Cnt === 6.U && pt2Cnt === 6.U) {
          state := State.WAIT_CORE
        }.otherwise {
          when(pt2Cnt === 6.U) {
            pt2Cnt := 0.U
            groupCnt := groupCnt + 1.U
            when(pt1Cnt === 6.U) {
              pt1Cnt := 0.U
              pt0Cnt := pt0Cnt + 1.U
            }.otherwise {
              pt1Cnt := pt1Cnt + 1.U
            }
          }.otherwise {
            pt2Cnt := pt2Cnt + 1.U
          }
        }
      }.otherwise {
        evalPhaseCnt := evalPhaseCnt + 1.U
      }
    }

    is(State.WAIT_CORE) {
      when(waitCoreCnt === 0.U) {
        waitCoreCnt := 1.U
      }.otherwise {
        waitCoreCnt := 0.U
        interpGroupCnt := 0.U
        interp1BlockCnt := 0.U
        interp1SubCnt   := 0.U
        state := State.INTERP1_START
      }
    }

    is(State.INTERP1_START) {
      interp16Seq.io.start := true.B
      state := State.INTERP1_WAIT
    }

    is(State.INTERP1_WAIT) {
      when(interp16Seq.io.done) {
        for (k <- 0 until 64) {
          regW1(interp1BlockCnt)(interp1SubCnt)(k) := interp16Seq.io.cOut(k)
        }
        when(interp1SubCnt === 6.U) {
          interp1SubCnt := 0.U
          when(interp1BlockCnt === 6.U) {
            interp1BlockCnt := 0.U
            interpGroupCnt := 0.U
            state := State.INTERP2_START
          }.otherwise {
            interp1BlockCnt := interp1BlockCnt + 1.U
            interpGroupCnt := interpGroupCnt + 1.U
            state := State.INTERP1_START
          }
        }.otherwise {
          interp1SubCnt := interp1SubCnt + 1.U
          interpGroupCnt := interpGroupCnt + 1.U
          state := State.INTERP1_START
        }
      }
    }

    is(State.INTERP2_START) {
      interp64Seq.io.start := true.B
      state := State.INTERP2_WAIT
    }

    is(State.INTERP2_WAIT) {
      when(interp64Seq.io.done) {
        for (k <- 0 until 256) {
          regW0(interpGroupCnt)(k) := interp64Seq.io.cOut(k)
        }
        when(interpGroupCnt === 6.U) {
          interpGroupCnt := 0.U
          state := State.INTERP3_START
        }.otherwise {
          interpGroupCnt := interpGroupCnt + 1.U
          state := State.INTERP2_START
        }
      }
    }

    is(State.INTERP3_START) {
      interp256Seq.io.start := true.B
      state := State.INTERP3_WAIT
    }

    is(State.INTERP3_WAIT) {
      when(interp256Seq.io.done) {
        for (i <- 0 until 1024) {
          regC(i) := BitUtil.mask(interp256Seq.io.cOut(i), 24)
        }
        state := State.DONE
      }
    }

    is(State.DONE) {
      io.valid_out := true.B
      state := State.IDLE
    }
  }
}
