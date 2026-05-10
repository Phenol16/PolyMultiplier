package core
import chisel3._
import chisel3.util._
class core64(
    aWidth: Int,
    bWidth: Int,
    aEvalWidth: Int,
    bEvalWidth: Int,
    core16OutWidth: Int,
    outWidth: Int,
    core16Core4InterpMk2: Int,
    core16Core4InterpMk3: Int,
    core16InterpMk: Int,
    core16InterpMk2: Int,
    core16InterpMk3: Int,
    interpMk: Int,
    interpMk2: Int,
    interpMk3: Int
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(64, UInt(aWidth.W)))
    val b = Input(Vec(64, UInt(bWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(64, UInt(outWidth.W)))
  })

  val A_eval = Wire(Vec(7 * 16, UInt(aEvalWidth.W)))
  val B_eval = Wire(Vec(7 * 16, UInt(bEvalWidth.W)))
  for (i <- 0 until 16) {
    val evalA = Module(new Eval4(aWidth, aEvalWidth))
    val evalB = Module(new Eval4(bWidth, bEvalWidth))
    for (seg <- 0 until 4) {
      evalA.io.in(seg) := io.a(seg * 16 + i)
      evalB.io.in(seg) := io.b(seg * 16 + i)
    }
    for (pt <- 0 until 7) {
      A_eval(pt * 16 + i) := evalA.io.out(pt)
      B_eval(pt * 16 + i) := evalB.io.out(pt)
    }
  }

  val core16 = Seq.fill(7)(
    Module(
      new core16(
        aWidth = aEvalWidth,
        bWidth = bEvalWidth,
        aEvalWidth = aEvalWidth,
        bEvalWidth = bEvalWidth,
        core4MulWidth = aEvalWidth,
        core4OutWidth = core16OutWidth,
        outWidth = core16OutWidth,
        core4InterpMk2 = core16Core4InterpMk2,
        core4InterpMk3 = core16Core4InterpMk3,
        interpMk = core16InterpMk,
        interpMk2 = core16InterpMk2,
        interpMk3 = core16InterpMk3
      )
    )
  )
  for (pt <- 0 until 7) {
    core16(pt).io.valid_in := io.valid_in
    for (i <- 0 until 16) {
      core16(pt).io.a(i) := A_eval(pt * 16 + i)
      core16(pt).io.b(i) := B_eval(pt * 16 + i)
    }
  }

  val core_valid = VecInit(core16.map(_.io.valid_out)).asUInt.andR
  val core_c_wire = Wire(Vec(7 * 16, UInt(core16OutWidth.W)))
  for (pt <- 0 until 7) {
    for (i <- 0 until 16) {
      core_c_wire(pt * 16 + i) := core16(pt).io.c(i)
    }
  }

  val s_valid = RegNext(core_valid, false.B)
  val s_w = RegNext(core_c_wire)

  val interp = Module(new Interpolation(
    stride = 16, 
    wWidth = core16OutWidth, 
    outWidth = outWidth, 
    mk = interpMk, 
    mk2 = interpMk2, 
    mk3 = interpMk3))
  interp.io.valid_in := s_valid
  interp.io.w := s_w

  io.valid_out := interp.io.valid_out
  io.c := interp.io.c
}