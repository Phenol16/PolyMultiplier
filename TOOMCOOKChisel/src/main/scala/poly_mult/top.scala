package poly_mult
import chisel3._
import chisel3.util._

class topIO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(1024, UInt(24.W)))
  val b = Input(Vec(1024, UInt(8.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(1024, UInt(24.W)))
}

class top extends Module {
  val io = IO(new topIO)

  val eval = Module(new EvalStage)
  val core = Module(new CoreStage)
  val interp = Module(new InterpStage)

  val qEvalToCore = Module(new Queue(new CoreInRecord, 2))
  val qCoreToInterp = Module(new Queue(new W2Record, 2))

  val busyReg = RegInit(false.B)
  val aLatched = Reg(Vec(1024, UInt(24.W)))
  val bLatched = Reg(Vec(1024, UInt(8.W)))

  val startPulse = Wire(Bool())
  startPulse := (!busyReg) && io.valid_in

  when(startPulse) {
    for (i <- 0 until 1024) {
      aLatched(i) := io.a(i)
      bLatched(i) := io.b(i)
    }
    busyReg := true.B
  }

  val evalAIn = Wire(Vec(1024, UInt(24.W)))
  val evalBIn = Wire(Vec(1024, UInt(8.W)))
  for (i <- 0 until 1024) {
    evalAIn(i) := Mux(startPulse, io.a(i), aLatched(i))
    evalBIn(i) := Mux(startPulse, io.b(i), bLatched(i))
  }

  // EvalStage
  eval.io.start := startPulse
  eval.io.aIn := evalAIn
  eval.io.bIn := evalBIn

  // Queue1
  qEvalToCore.io.enq <> eval.io.out

  // CoreStage
  core.io.in <> qEvalToCore.io.deq

  // Queue2
  qCoreToInterp.io.enq <> core.io.out

  // InterpStage
  interp.io.in <> qCoreToInterp.io.deq

  // OutputRegister
  val cOutReg = Reg(Vec(1024, UInt(24.W)))
  val validOutReg = RegInit(false.B)

  validOutReg := false.B
  when(interp.io.out.valid) {
    for (i <- 0 until 1024) {
      cOutReg(i) := interp.io.out.bits(i)
    }
    validOutReg := true.B
    busyReg := false.B
  }

  io.valid_out := validOutReg
  io.c := cOutReg
}
