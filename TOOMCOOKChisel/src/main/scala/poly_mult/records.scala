package poly_mult

import chisel3._

class CoreInRecord extends Bundle {
  val seg = UInt(9.W)
  val pt0 = UInt(3.W)
  val pt1 = UInt(3.W)
  val pt2 = UInt(3.W)
  val aEval = Vec(16, UInt(30.W))
  val bEval = Vec(16, UInt(16.W))
}

class W2Record extends Bundle {
  val seg = UInt(9.W)
  val pt0 = UInt(3.W)
  val pt1 = UInt(3.W)
  val pt2 = UInt(3.W)
  val data = Vec(16, UInt(27.W))
}