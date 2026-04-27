package poly_mult

import chisel3._

object generator extends App {
  println("Generating the hardware")
  emitVerilog(new top(), Array("--target-dir", "generated/top"))
}
