package core
import chisel3._
import chisel3.util._
class core4(
    aWidth: Int,
    bWidth: Int,
    mulWidth: Int,
    outWidth: Int,
    interpMk2: Int = -1,
    interpMk3: Int = -1
) extends Module {
  private val actualInterpMk2 = if (interpMk2 > 0) interpMk2 else mulWidth
  private val actualInterpMk3 = if (interpMk3 > 0) interpMk3 else mulWidth
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(4, UInt(aWidth.W)))
    val b = Input(Vec(4, UInt(bWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(4, UInt(outWidth.W)))
  })

  val evalA = Module(new Eval4(aWidth, aWidth))
  val evalB = Module(new Eval4(bWidth, bWidth))
  evalA.io.in := io.a
  evalB.io.in := io.b

  val w = Wire(Vec(7, UInt(mulWidth.W)))
  for (i <- 0 until 7) {
    w(i) := ParaMath.signedMulMod(
    evalA.io.out(i),
    aWidth, 
    evalB.io.out(i), 
    bWidth, 
    mulWidth)
  }

  val s_valid = RegNext(io.valid_in, false.B)
  val s_w = RegNext(w)

  val interp = Module(new Interpolation(
  stride = 1, 
  wWidth = mulWidth, 
  outWidth = outWidth, 
  mk = mulWidth, 
  mk2 = actualInterpMk2, 
  mk3 = actualInterpMk3))
  interp.io.valid_in := s_valid
  interp.io.w := s_w

  io.valid_out := interp.io.valid_in
  io.c := interp.io.c
}