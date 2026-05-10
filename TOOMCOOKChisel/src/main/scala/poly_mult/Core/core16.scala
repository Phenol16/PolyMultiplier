package core
import chisel3._
import chisel3.util._
class core16(
    aWidth: Int,
    bWidth: Int,
    aEvalWidth: Int,
    bEvalWidth: Int,
    core4MulWidth: Int,
    core4OutWidth: Int,
    outWidth: Int,
    core4InterpMk2: Int,
    core4InterpMk3: Int,
    interpMk: Int,
    interpMk2: Int,
    interpMk3: Int
) extends Module {

  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(16, UInt(aWidth.W)))
    val b = Input(Vec(16, UInt(bWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(16, UInt(outWidth.W)))
  })

    val A_eval = Wire(Vec(7 * 4, UInt(aEvalWidth.W)))
    val B_eval = Wire(Vec(7 * 4, UInt(bEvalWidth.W)))
    for (i <- 0 until 4) {
    val evalA = Module(new Eval4(aWidth, aEvalWidth))
    val evalB = Module(new Eval4(bWidth, bEvalWidth))

    for (seg <- 0 until 4) {
      evalA.io.in(seg) := io.a(seg * 4 + i)
      evalB.io.in(seg) := io.b(seg * 4 + i)
    }

    for (pt <- 0 until 7) {
      A_eval(pt * 4 + i) := evalA.io.out(pt)
      B_eval(pt * 4 + i) := evalB.io.out(pt)
    }
  }

  val core4 = Seq.fill(7)(Module(new core4(
    aEvalWidth, 
    bEvalWidth, 
    core4MulWidth, 
    core4OutWidth, 
    core4InterpMk2, 
    core4InterpMk3)))
  for (pt <- 0 until 7) {
    core4(pt).io.valid_in := io.valid_in
    for (i <- 0 until 4) {
      core4(pt).io.a(i) := A_eval(pt * 4 + i)
      core4(pt).io.b(i) := B_eval(pt * 4 + i)
    }
  }

  val core_valid = VecInit(core4.map(_.io.valid_out)).asUInt.andR
  val core_c_wire = Wire(Vec(7 * 4, UInt(core4OutWidth.W)))
  for (pt <- 0 until 7) {
    for (i <- 0 until 4) {
      core_c_wire(pt * 4 + i) := core4(pt).io.c(i)
    }
  }
  val s_valid = RegNext(core_valid, false.B)
  val s_w = RegNext(core_c_wire)

  val interp = Module(new Interpolation(
    stride = 4, 
    wWidth = core4OutWidth, 
    outWidth = outWidth, 
    mk = interpMk, 
    mk2 = interpMk2, 
    mk3 = interpMk3))
  interp.io.valid_in := s_valid
  interp.io.w := s_w

  io.valid_out := interp.io.valid_out
  io.c := interp.io.c
}