package poly_mult

import chisel3._
import chisel3.util._

class EvalPoint(val inW: Int, val outW: Int) extends Module {
  val io = IO(new Bundle {
    val r0 = Input(UInt(inW.W))
    val r1 = Input(UInt(inW.W))
    val r2 = Input(UInt(inW.W))
    val r3 = Input(UInt(inW.W))
    val pt = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  val even = io.r0 +& io.r2
  val odd = io.r1 +& io.r3

  val scaledEven = Cat(io.r0, 0.U(2.W)) +& io.r2
  val scaledOdd = Cat(io.r1, 0.U(2.W)) +& io.r3

  val v0 = io.r3
  val v1 = io.r0 +& (io.r1 << 1) +& (io.r2 << 2) +& (io.r3 << 3)
  val v2 = even +& odd
  val sub = even -& odd
  val v3 = if (outW > sub.getWidth) {
    Cat(Fill(outW - sub.getWidth, sub(sub.getWidth - 1)), sub)
  } else {
    sub(outW - 1, 0)
  }
  val v4 = Cat(scaledEven, 0.U(1.W)) +& scaledOdd
  val sub2 = Cat(scaledEven, 0.U(1.W)) -& scaledOdd
  val v5 = if (outW > sub2.getWidth) {
    Cat(Fill(outW - sub2.getWidth, sub2(sub2.getWidth - 1)), sub2)
  } else {
    sub2(outW - 1, 0)
  }
  val v6 = io.r0

  io.out := MuxLookup(
    io.pt,
    0.U(outW.W),
    Seq(
      0.U -> v0(outW - 1, 0),
      1.U -> v1(outW - 1, 0),
      2.U -> v2(outW - 1, 0),
      3.U -> v3(outW - 1, 0),
      4.U -> v4(outW - 1, 0),
      5.U -> v5(outW - 1, 0),
      6.U -> v6(outW - 1, 0)
    )
  )
}

class EvalStageIO extends Bundle {
  val start = Input(Bool())
  val aIn = Input(Vec(1024, UInt(24.W)))
  val bIn = Input(Vec(1024, UInt(8.W)))
  val out = Decoupled(new CoreInRecord)
  val busy = Output(Bool())
}

class EvalStage extends Module {
  val io = IO(new EvalStageIO)

  val sIdle :: sRun :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val aReg = Reg(Vec(1024, UInt(24.W)))
  val bReg = Reg(Vec(1024, UInt(8.W)))

  val seg = RegInit(0.U(9.W))
  val pt0 = RegInit(0.U(3.W))
  val pt1 = RegInit(0.U(3.W))
  val pt2 = RegInit(0.U(3.W))

  val recWire = Wire(new CoreInRecord)
  recWire.seg := seg
  recWire.pt0 := pt0
  recWire.pt1 := pt1
  recWire.pt2 := pt2

  for (l <- 0 until 16) {
    val lv2A = Wire(Vec(4, UInt(30.W)))
    val lv2B = Wire(Vec(4, UInt(16.W)))

    for (k <- 0 until 4) {
      val lv1A = Wire(Vec(4, UInt(30.W)))
      val lv1B = Wire(Vec(4, UInt(16.W)))

      for (j <- 0 until 4) {
        val idx0 = 64 * l + 16 * k + 4 * j

        val epA = Module(new EvalPoint(24, 30))
        epA.io.r0 := aReg(idx0 + 0)
        epA.io.r1 := aReg(idx0 + 1)
        epA.io.r2 := aReg(idx0 + 2)
        epA.io.r3 := aReg(idx0 + 3)
        epA.io.pt := pt0
        lv1A(j) := epA.io.out

        val epB = Module(new EvalPoint(8, 16))
        epB.io.r0 := bReg(idx0 + 0)
        epB.io.r1 := bReg(idx0 + 1)
        epB.io.r2 := bReg(idx0 + 2)
        epB.io.r3 := bReg(idx0 + 3)
        epB.io.pt := pt0
        lv1B(j) := epB.io.out
      }

      val epA2 = Module(new EvalPoint(24, 30))
      epA2.io.r0 := lv1A(0)(23, 0)
      epA2.io.r1 := lv1A(1)(23, 0)
      epA2.io.r2 := lv1A(2)(23, 0)
      epA2.io.r3 := lv1A(3)(23, 0)
      epA2.io.pt := pt1
      lv2A(k) := epA2.io.out

      val epB2 = Module(new EvalPoint(8, 16))
      epB2.io.r0 := lv1B(0)(7, 0)
      epB2.io.r1 := lv1B(1)(7, 0)
      epB2.io.r2 := lv1B(2)(7, 0)
      epB2.io.r3 := lv1B(3)(7, 0)
      epB2.io.pt := pt1
      lv2B(k) := epB2.io.out
    }

    val epA3 = Module(new EvalPoint(24, 30))
    epA3.io.r0 := lv2A(0)(23, 0)
    epA3.io.r1 := lv2A(1)(23, 0)
    epA3.io.r2 := lv2A(2)(23, 0)
    epA3.io.r3 := lv2A(3)(23, 0)
    epA3.io.pt := pt2
    recWire.aEval(l) := epA3.io.out

    val epB3 = Module(new EvalPoint(8, 16))
    epB3.io.r0 := lv2B(0)(7, 0)
    epB3.io.r1 := lv2B(1)(7, 0)
    epB3.io.r2 := lv2B(2)(7, 0)
    epB3.io.r3 := lv2B(3)(7, 0)
    epB3.io.pt := pt2
    recWire.bEval(l) := epB3.io.out
  }

  io.out.valid := (state === sRun)
  io.out.bits := recWire
  io.busy := (state === sRun)

  when(state === sIdle) {
    when(io.start) {
      for (i <- 0 until 1024) {
        aReg(i) := io.aIn(i)
        bReg(i) := io.bIn(i)
      }
      seg := 0.U
      pt0 := 0.U
      pt1 := 0.U
      pt2 := 0.U
      state := sRun
    }
  }.otherwise {
    when(io.out.fire) {
      when(seg === 342.U) {
        state := sIdle
      }.otherwise {
        seg := seg + 1.U
        when(pt2 === 6.U) {
          pt2 := 0.U
          when(pt1 === 6.U) {
            pt1 := 0.U
            pt0 := pt0 + 1.U
          }.otherwise {
            pt1 := pt1 + 1.U
          }
        }.otherwise {
          pt2 := pt2 + 1.U
        }
      }
    }
  }
}