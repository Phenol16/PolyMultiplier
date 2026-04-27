package poly_mult

import chisel3._
import chisel3.util._

class EvalPoint(val inW: Int, val outW: Int) extends Module {
  val io = IO(new Bundle {
    val r0  = Input(UInt(inW.W))
    val r1  = Input(UInt(inW.W))
    val r2  = Input(UInt(inW.W))
    val r3  = Input(UInt(inW.W))
    val pt  = Input(UInt(3.W))
    val out = Output(UInt(outW.W))
  })

  private def fillMsb(value: UInt, targetWidth: Int): UInt = {
    if (value.getWidth >= targetWidth) {
      value(targetWidth - 1, 0)
    } else {
      Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
    }
  }
  val even = io.r0 +& io.r2
  val odd  = io.r1 +& io.r3

  val scaled_even = Cat(io.r0, 0.U(2.W)) +& io.r2
  val scaled_odd  = Cat(io.r1, 0.U(2.W)) +& io.r3

  val h0 = io.r2 +& Cat(io.r3, 0.U(1.W))
  val h1 = io.r1 +& Cat(h0, 0.U(1.W))
  val h2 = io.r0 +& Cat(h1, 0.U(1.W))

  val v_inf = io.r3

  val v_pos2 = h2

  val v_pos1 = even +& odd

  val v_neg1 = fillMsb(
    even -& odd,
    outW
  )

  val v_pos_half = Cat(scaled_even, 0.U(1.W)) +& scaled_odd

  val v_neg_half = fillMsb(
    Cat(scaled_even, 0.U(1.W)) -& scaled_odd,
    outW
  )

  val v_zero = io.r0

  val out_wire = WireDefault(0.U(outW.W))

  switch(io.pt) {
    is(0.U) {
      out_wire := v_inf
    }

    is(1.U) {
      out_wire := v_pos2
    }

    is(2.U) {
      out_wire := v_pos1
    }

    is(3.U) {
      out_wire := v_neg1
    }

    is(4.U) {
      out_wire := v_pos_half
    }

    is(5.U) {
      out_wire := v_neg_half
    }

    is(6.U) {
      out_wire := v_zero
    }
  }

  io.out := out_wire
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