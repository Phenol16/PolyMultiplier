package poly_mult
import chisel3._
import chisel3.util._

class interpIO extends Bundle {
  val valid_in = Input(Bool())
  val w = Input(Vec(28, UInt(27.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(16, UInt(24.W)))
}

class interpolation extends Module {}
