package poly_mult_sram

import chisel3._
import chisel3.util._

// This file currently centralizes SRAM/Reg-buffer specifications only.
// Future foundry SRAM macro wrappers can be generated/mapped from these RamSpec entries.
// SpRam remains a behavioral BlackBox/simulation model for now, and does not imply final macro mapping.
class SpRam(width: Int, depth: Int) extends BlackBox(Map("WIDTH" -> width, "DEPTH" -> depth)) with HasBlackBoxResource {
  override def desiredName: String = "sp_ram"
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val en = Input(Bool())
    val we = Input(Bool())
    val addr = Input(UInt(log2Ceil(depth).W))
    val din = Input(UInt(width.W))
    val dout = Output(UInt(width.W))
  })
  addResource("/sp_ram.v")
}

object ToomCook1024SramSpec {
  case class RamSpec(
    logicalName: String,
    width: Int,
    depth: Int,
    banks: Int,
    recommendedImpl: String,
    note: String
  )

  val InA = RamSpec(
    logicalName = "inARam",
    width = 64 * 24,
    depth = 16,
    banks = 1,
    recommendedImpl = "SRAM macro, suggested wrapper sram_1536x16; can be built from 16 x 32x96 macros",
    note = "Input A buffer, 64 coefficients per word"
  )

  val InB = RamSpec(
    logicalName = "inBRam",
    width = 64 * 8,
    depth = 16,
    banks = 1,
    recommendedImpl = "SRAM macro, suggested wrapper sram_512x16; can be built from 32x96/32x16 macros",
    note = "Input B buffer, 64 coefficients per word"
  )

  val EvalA = RamSpec(
    logicalName = "evalARam",
    width = 16 * 39,
    depth = 172,
    banks = 2,
    recommendedImpl = "SRAM macro, suggested wrapper sram_624x172; can be built from 256x96 and 256x24 macros",
    note = "Two-bank evaluation buffer for A"
  )

  val EvalB = RamSpec(
    logicalName = "evalBRam",
    width = 16 * 29,
    depth = 172,
    banks = 2,
    recommendedImpl = "SRAM macro, suggested wrapper sram_464x172; can be built from 256x96 macros with unused upper bits",
    note = "Two-bank evaluation buffer for B"
  )

  val Core = RamSpec(
    logicalName = "coreRam",
    width = 16 * 36,
    depth = 25,
    banks = 2 * 7,
    recommendedImpl = "SRAM macro, suggested wrapper sram_576x25; can be built from 32x96 macros",
    note = "Ping-pong page buffer for core16 outputs"
  )

  val Out = RamSpec(
    logicalName = "outRam",
    width = 64 * 24,
    depth = 16,
    banks = 1,
    recommendedImpl = "SRAM macro, suggested wrapper sram_1536x16; can be built from 16 x 32x96 macros",
    note = "Final output buffer, 64 coefficients per word"
  )

  val W1 = RamSpec(
    logicalName = "w1Buf",
    width = 16 * 33,
    depth = 1,
    banks = 2 * 7 * 4,
    recommendedImpl = "Reg buffer",
    note = "W1 has no useful address dimension in fixed 16-column design"
  )

  val W0 = RamSpec(
    logicalName = "w0Buf",
    width = 16 * 27,
    depth = 4,
    banks = 7 * 4,
    recommendedImpl = "Reg buffer for current implementation",
    note = "Depth=4 is too shallow for available SRAM macros; using Reg avoids wasting 32-depth macro rows. If area is too large, consider custom register-file or latch-based memory later."
  )
}