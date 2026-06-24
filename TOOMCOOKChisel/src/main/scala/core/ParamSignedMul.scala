package core

import chisel3._

class ParamSignedMul(aW: Int, bW: Int, outW: Int) extends Module {
  require(aW > 0, "aW must be positive")
  require(bW > 0, "bW must be positive")
  require(outW > 0, "outW must be positive")

  val io = IO(new Bundle {
    val a = Input(UInt(aW.W))
    val b = Input(UInt(bW.W))
    val c = Output(UInt(outW.W))
  })

  // Interpret both UInt inputs as two's-complement signed coefficients, multiply
  // with SInt semantics, and truncate the result modulo 2^outW.
  val aExt = CoreBitUtil.signExtend(io.a, aW + 1).asSInt
  val bExt = CoreBitUtil.signExtend(io.b, bW + 1).asSInt
  val prod = aExt * bExt
  io.c := CoreBitUtil.mask(prod.asUInt, outW)
}
