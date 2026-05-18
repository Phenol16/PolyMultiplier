package poly_mult_sram

import chisel3._
import chisel3.util._

class SramIO(width: Int, depth: Int) extends Bundle {
  val clk  = Input(Clock())
  val en   = Input(Bool())
  val we   = Input(Bool())
  val addr = Input(UInt(log2Ceil(depth).W))
  val din  = Input(UInt(width.W))
  val dout = Output(UInt(width.W))
}

abstract class FixedSram(width: Int, depth: Int, moduleName: String)
    extends BlackBox {
  override def desiredName: String = moduleName
  val io = IO(new SramIO(width, depth))
}

class Sram1536x16 extends FixedSram(1536, 16, "sram_1536x16")
class Sram512x16  extends FixedSram(512, 16, "sram_512x16")
class Sram624x172 extends FixedSram(624, 172, "sram_624x172")
class Sram464x172 extends FixedSram(464, 172, "sram_464x172")
class Sram576x25  extends FixedSram(576, 25, "sram_576x25")
