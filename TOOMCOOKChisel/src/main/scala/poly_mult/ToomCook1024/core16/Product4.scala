package poly_mult

import chisel3._
import chisel3.util._

// Product4：4系数 × 4系数 -> 7系数，纯组合硬件模块
class Product4 extends Module {
  private val A_EVAL_W = PolyMulConfig.A_EVAL_W
  private val B_EVAL_W = PolyMulConfig.B_EVAL_W
  private val PROD_MUL_MOD_W = A_EVAL_W
  private val PROD_OUT_W = PolyMulConfig.CORE_OUT_W

  val io = IO(new Bundle {
    val a4  = Input(Vec(4, UInt(A_EVAL_W.W)))
    val b4  = Input(Vec(4, UInt(B_EVAL_W.W)))
    val out = Output(Vec(7, UInt(PROD_OUT_W.W)))
  })

  val evalA = Module(new EvalLayer(A_EVAL_W, A_EVAL_W))
  val evalB = Module(new EvalLayer(B_EVAL_W, B_EVAL_W))

  evalA.io.r := io.a4
  evalB.io.r := io.b4

  val wMul = Wire(Vec(7, UInt(PROD_MUL_MOD_W.W)))

  for (i <- 0 until 7) {
    val bw     = evalB.io.out(i)(B_EVAL_W - 1, 0)
    val bwSign = bw(B_EVAL_W - 1)
    val bwSext = Cat(Fill(A_EVAL_W - B_EVAL_W, bwSign), bw).asSInt
    val awInt  = evalA.io.out(i)(A_EVAL_W - 1, 0).asSInt
    wMul(i) := BitUtil.mask((awInt * bwSext).asUInt, PROD_MUL_MOD_W)
  }

  val r5a = BitUtil.mask(wMul(5) - wMul(4), PROD_MUL_MOD_W)
  val r3a = BitUtil.mask(BitUtil.mask(wMul(3) - wMul(2), PROD_MUL_MOD_W) >> 1, PROD_MUL_MOD_W)
  val r4a = BitUtil.mask(wMul(4) - wMul(0), PROD_MUL_MOD_W)
  val r4b = BitUtil.mask((r4a << 1) + r5a - (wMul(6) << 7), PROD_MUL_MOD_W)
  val r2a = BitUtil.mask(wMul(2) + r3a, PROD_MUL_MOD_W)
  val r1a = BitUtil.mask(wMul(1) + wMul(4) - (r2a << 6) - r2a, PROD_MUL_MOD_W)
  val r2b = BitUtil.mask(r2a - wMul(6) - wMul(0), PROD_MUL_MOD_W)
  val r1b = BitUtil.mask(r1a + r2b + (r2b << 2) + (r2b << 3) + (r2b << 5), PROD_MUL_MOD_W)

  val r4c = BitUtil.mask(
    BitUtil.mask(BitUtil.mask(r4b - (r2b << 3), PROD_MUL_MOD_W) >> 3, PROD_MUL_MOD_W) * "hAAAAAAAAB".U(42.W), PROD_OUT_W
  )
  val r5b = BitUtil.mask(
    BitUtil.mask((r5a + r1b) >> 1, PROD_MUL_MOD_W) * "hEEEEEEEEF".U(42.W), 37
  )
  val r1c = BitUtil.mask(
    BitUtil.mask(BitUtil.mask(r1b + (r3a << 4), PROD_MUL_MOD_W) >> 1, PROD_MUL_MOD_W) * "hE38E38E39".U(42.W), 37
  )

  val r2c = BitUtil.mask(r2b - r4c, PROD_OUT_W)
  val r3b = BitUtil.mask(0.U - r3a - r1c, PROD_OUT_W)
  val r5c = BitUtil.mask((r1c - r5b) >> 1, PROD_OUT_W)
  val r1d = BitUtil.mask(r1c - r5c, PROD_OUT_W)

  io.out(0) := BitUtil.mask(wMul(6) - r2c, PROD_OUT_W)
  io.out(1) := BitUtil.mask(r5c - r1d, PROD_OUT_W)
  io.out(2) := BitUtil.mask(r4c - wMul(0), PROD_OUT_W)
  io.out(3) := r3b
  io.out(4) := 0.U
  io.out(5) := 0.U
  io.out(6) := 0.U
}
