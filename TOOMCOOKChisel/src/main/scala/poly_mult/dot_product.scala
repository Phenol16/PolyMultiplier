package poly_mult
import chisel3._
import chisel3.util._

class dotIO extends Bundle {
  val valid_in = Input(Bool())
  val a = Input(Vec(4, UInt(30.W)))
  val b = Input(Vec(4, UInt(16.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(4, UInt(27.W)))
}
class dot_product extends Module {
  val io = IO(new dotIO)
}
