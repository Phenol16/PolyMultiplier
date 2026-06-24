package core

import chisel3._
import chisel3.util.Fill

object CoreBitUtil {
  def mask(value: UInt, targetWidth: Int): UInt = {
    require(targetWidth > 0, "mask width must be positive")
    if (value.getWidth >= targetWidth) {
      value(targetWidth - 1, 0)
    } else {
      Cat(Fill(targetWidth - value.getWidth, 0.U), value)
    }
  }

  def signExtend(value: UInt, targetWidth: Int): UInt = {
    require(targetWidth > 0, "target width must be positive")
    require(value.getWidth > 0, "source width must be positive")
    if (value.getWidth >= targetWidth) {
      value(targetWidth - 1, 0)
    } else {
      Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
    }
  }

  def maskConst(width: Int): UInt = {
    require(width > 0, "mask width must be positive")
    ((BigInt(1) << width) - 1).U(width.W)
  }
}
