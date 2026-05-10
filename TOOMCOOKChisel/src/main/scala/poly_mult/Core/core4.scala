package core
import chisel3._
import chisel3.util._
class core4(
    aWidth: Int,
    bWidth: Int,
    evalGrowth:Int=5,
    inteGrowth:Int=3,
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a = Input(Vec(4, UInt(aWidth.W)))
    val b = Input(Vec(4, UInt(bWidth.W)))
    val valid_out = Output(Bool())
    val c = Output(Vec(4, UInt(aWidth.W)))
  })

  val evalA = Module(new Eval(inWidth = aWidth, outWidth = (aWidth+inteGrowth)))
  val evalB = Module(new Eval(inWidth = bWidth, outWidth = (bWidth+evalGrowth)))
  evalA.io.in := io.a
  evalB.io.in := io.b

  val w = Wire(Vec(7, UInt((aWidth+inteGrowth).W)))
  for (i <- 0 until 7) {
    val aSigned = evalA.io.out(i).asSInt
    //val bSigned = Cat(evalB.io.out(i)((bWidth+evalGrowth)-1), evalB.io.out(i)).asSInt
    val bSigned = evalB.io.out(i).asSInt
    w(i) := (aSigned *bSigned).asUInt
  }

  val s_valid = RegNext(io.valid_in, false.B)
  val s_w = RegNext(w)

  val interp = Module(new Interpolation(
  stride = 1, 
  inWidth = (aWidth+inteGrowth), 
  outWidth = aWidth))

  interp.io.valid_in := s_valid
  interp.io.w := s_w

  io.valid_out := interp.io.valid_out
  io.c := interp.io.c
}