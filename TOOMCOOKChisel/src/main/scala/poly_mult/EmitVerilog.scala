package poly_mult

import chisel3._

object generator extends App {
  println("Generating the hardware")
  //emitVerilog(new ToomCook44()  , Array("--target-dir", "generated/ToomCook44"))
  emitVerilog(new kernel()  , Array("--target-dir", "generated/kernel"))
}
