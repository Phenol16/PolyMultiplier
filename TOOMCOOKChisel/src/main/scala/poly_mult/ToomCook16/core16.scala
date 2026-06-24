package poly_mult
import chisel3._
import chisel3.util._

class core16IO(
    aWidth: Int = 24,
    bWidth: Int = 8,
    outWidth: Int = 24
) extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(16, UInt(aWidth.W)))
  val b = Input(Vec(16, UInt(bWidth.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(16, UInt(outWidth.W)))
}

class core16(
    aWidth: Int = 24,
    bWidth: Int = 8,
    aEvalWidth: Int = 30,
    bEvalWidth: Int = 16,
    core4MulWidth: Int = 30,
    core4OutWidth: Int = 27,
    outWidth: Int = 24,
    interpMk: Int = 27,
    interpMk2: Int = 24,
    interpMk3: Int = 25,
    core4Inv3: BigInt = BigInt("2AAAAAAB", 16),
    core4Inv9: BigInt = BigInt("38E38E39", 16),
    core4Inv18: BigInt = BigInt("2EEEEEEF", 16),
    interpInv3: BigInt = BigInt("AAAAAAB", 16),
    interpInv9: BigInt = BigInt("E38E39", 16),
    interpInv18: BigInt = BigInt("EEEEEF", 16)
) extends Module {
  val io = IO(new core16IO(aWidth, bWidth, outWidth))

  // ========== evaluation ==========
  val eval = Module(new evaluation(aWidth, bWidth, aEvalWidth, bEvalWidth))
  eval.io.valid_in := io.valid_in
  eval.io.a := io.a
  eval.io.b := io.b

  // ========== 7x core4 (parallel) ==========
  val dots = Seq.fill(7)(
    Module(
      new core4(
        aWidth = aEvalWidth,
        bWidth = bEvalWidth,
        mulWidth = core4MulWidth,
        outWidth = core4OutWidth,
        inv3 = core4Inv3,
        inv9 = core4Inv9,
        inv18 = core4Inv18
      )
    )
  )
  for (i <- 0 until 7) {
    dots(i).io.valid_in := eval.io.valid_out
    for (j <- 0 until 4) {
      dots(i).io.a(j) := eval.io.A_eval(i * 4 + j)
      dots(i).io.b(j) := eval.io.B_eval(i * 4 + j)
    }
  }

  // 打一拍：core4 → interpolation
  val dot_valid_reg = RegNext(VecInit(dots.map(_.io.valid_out)).asUInt.andR, false.B)
  val dot_c_wire = Wire(Vec(28, UInt(core4OutWidth.W)))
  for (i <- 0 until 7) {
    for (j <- 0 until 4) {
      dot_c_wire(i * 4 + j) := dots(i).io.c(j)
    }
  }
  val dot_c_reg = RegNext(dot_c_wire)

  // ========== interpolation ==========
  val interp = Module(
    new interpolation(
      wWidth = core4OutWidth,
      outWidth = outWidth,
      stride = 4,
      mk = interpMk,
      mk2 = interpMk2,
      mk3 = interpMk3,
      inv3 = interpInv3,
      inv9 = interpInv9,
      inv18 = interpInv18
    )
  )
  interp.io.valid_in := dot_valid_reg
  interp.io.w := dot_c_reg

  // ========== Output ==========
  io.valid_out := interp.io.valid_out
  io.c := interp.io.c
}
