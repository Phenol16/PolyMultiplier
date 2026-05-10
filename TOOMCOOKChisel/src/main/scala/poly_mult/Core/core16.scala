package core
import chisel3._
import chisel3.util._
class core16(
    aWidth: Int,
    bWidth: Int,
    evalGrowth:Int=3,
    inteGrowth:Int=4,
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(16, UInt(aWidth.W)))
    val b = Input(Vec(16, UInt(bWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(16, UInt(aWidth.W)))
  })

    val A_eval = Wire(Vec(7 * 4, UInt((aWidth + inteGrowth).W)))
    val B_eval = Wire(Vec(7 * 4, UInt((bWidth + evalGrowth).W)))
    for (j <- 0 until 4) {
  val evalA = Module(new Eval(inWidth = aWidth, outWidth = (aWidth+inteGrowth)))
  val evalB = Module(new Eval(inWidth = bWidth, outWidth = (bWidth+evalGrowth)))

    for (i <- 0 until 4) {
      evalA.io.in(i) := io.a(j * 4 + i)
      evalB.io.in(i) := io.b(j * 4 + i)
    }
    for (pt <- 0 until 7) {
      A_eval(pt * 4 + j) := evalA.io.out(pt)
      B_eval(pt * 4 + j) := evalB.io.out(pt)
    }
  }

  val core4 = Seq.fill(7)(
    Module(new core4(aWidth = (aWidth+inteGrowth),bWidth = (bWidth+evalGrowth)))
    )
  for (pt <- 0 until 7) {
    core4(pt).io.valid_in := io.valid_in
    for (i <- 0 until 4) {
      core4(pt).io.a(i) := A_eval(pt * 4 + i)
      core4(pt).io.b(i) := B_eval(pt * 4 + i)
    }
  }
  val core_valid = VecInit(core4.map(_.io.valid_out)).asUInt.andR
  val core_c_wire = Wire(Vec(7 * 4, UInt((aWidth+inteGrowth).W)))
  for (pt <- 0 until 7) {
    for (i <- 0 until 4) {
      core_c_wire(pt * 4 + i) := core4(pt).io.c(i)
    }
  }
  val s_valid = RegNext(core_valid, false.B)
  val s_w = RegNext(core_c_wire)

  val interp = Module(new Interpolation(
    stride = 4, 
    inWidth = (aWidth+inteGrowth), 
    outWidth = aWidth))

  interp.io.valid_in := s_valid
  interp.io.w := s_w

  io.valid_out := interp.io.valid_out
  io.c := interp.io.c
}