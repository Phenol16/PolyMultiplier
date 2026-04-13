package poly_mult
import chisel3._
import chisel3.util._

class kernelIO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(16, UInt(24.W)))
  val b = Input(Vec(16, UInt(8.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(16, UInt(24.W)))
}

class kernel extends Module {
  val io = IO(new kernelIO)

  // ========== Stage 1: evaluation ==========
  val eval = Module(new evaluation)
  eval.io.valid_in := io.valid_in
  eval.io.a := io.a
  eval.io.b := io.b

  // ========== Reg 1: eval → dot ==========
  val eval_valid_reg = RegNext(eval.io.valid_out, false.B)
  val A_eval_reg = RegNext(eval.io.A_eval)
  val B_eval_reg = RegNext(eval.io.B_eval)

  // ========== Stage 2: 7x dot_product (parallel) ==========
  val dots = Seq.fill(7)(Module(new dot_product))

  for (i <- 0 until 7) {
    dots(i).io.valid_in := eval_valid_reg
    for (j <- 0 until 4) {
      dots(i).io.a(j) := A_eval_reg(i * 4 + j)
      dots(i).io.b(j) := B_eval_reg(i * 4 + j)
    }
  }

  // ========== Reg 2: dot → interp ==========
  val dot_valid_reg = RegNext(dots(0).io.valid_out, false.B)
  val dot_c_wire = Wire(Vec(28, UInt(27.W)))
  for (i <- 0 until 7) {
    for (j <- 0 until 4) {
      dot_c_wire(i * 4 + j) := dots(i).io.c(j)

    }
  }
  val dot_c_reg = RegNext(dot_c_wire)

  /*   when(dot_valid_reg) {
    printf("--- Stage 2 7x dot_product ---\n")
    for (i <- 0 until 7) {
      printf(
        "Point %d, a = [%x,%x,%x,%x],b = [%x,%x,%x,%x],w = [%x,%x,%x,%x]\n",
        i.U,
        A_eval_reg(i * 4 + 0),
        A_eval_reg(i * 4 + 1),
        A_eval_reg(i * 4 + 2),
        A_eval_reg(i * 4 + 3),
        B_eval_reg(i * 4 + 0),
        B_eval_reg(i * 4 + 1),
        B_eval_reg(i * 4 + 2),
        B_eval_reg(i * 4 + 3),
        dot_c_reg(i * 4 + 0),
        dot_c_reg(i * 4 + 1),
        dot_c_reg(i * 4 + 2),
        dot_c_reg(i * 4 + 3)
      )
    }
  } */

  // ========== Stage 3: interpolation ==========
  val interp = Module(new interpolation)
  interp.io.valid_in := dot_valid_reg
  interp.io.w := dot_c_reg
  when(interp.io.valid_out) {
    printf("--- Stage 3 interpolation ---\n")
    for (i <- 0 until 16) {
      printf("c[%d]=%x\n", i.U, interp.io.c(i))
    }
  }
  // ========== Output ==========
  io.valid_out := interp.io.valid_out
  io.c := interp.io.c

}
