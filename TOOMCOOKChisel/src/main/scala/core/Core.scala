package core
import chisel3._
import chisel3.util._

object CoreUtil {
  def mask(value: UInt, targetWidth: Int): UInt = {
    require(targetWidth > 0, "mask width must be positive")
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, 0.U), value)
  }

  def fillMsb(value: UInt, targetWidth: Int): UInt = {
    require(targetWidth > 0, "fillMsb width must be positive")
    if (value.getWidth >= targetWidth) value(targetWidth - 1, 0)
    else Cat(Fill(targetWidth - value.getWidth, value(value.getWidth - 1)), value)
  }

  def signedResize(value: SInt, targetWidth: Int): SInt = fillMsb(value.asUInt, targetWidth).asSInt
  def signedToUInt(value: SInt, targetWidth: Int): UInt = mask(value.asUInt, targetWidth)
}

/** 插值参数表。
  * stride=1/4/16 分别对应 degree=4/16/64 的 TC4 插值层。
  * mk/mk2/mk3 按当前层 inW/outW 动态生成；inv* 保留表格字段，便于以后替换为
  * 旧版模逆乘法实现。本实现 InterpCoreTC4 使用显式有符号常数除法，避免把 36/33/30
  * 等旧 magic number 写死到参数化 Core 中。
  */
object InterpParamTable {
  case class Param(stride: Int, pidx: Int, mk: Int, mk2: Int, mk3: Int,
                   inv3: BigInt, inv9: BigInt, inv18: BigInt)

  def pidxForStride(stride: Int): Int = stride match {
    case 1  => 0
    case 4  => 1
    case 16 => 2
    case other => throw new IllegalArgumentException(s"unsupported TC4 interpolation stride $other")
  }

  def params(stride: Int, inW: Int, outW: Int): Param = {
    val pidx = pidxForStride(stride)
    Param(stride, pidx, inW + 16, outW, outW + 1, BigInt(0), BigInt(0), BigInt(0))
  }
}

/** TC4 求值层：4 个系数 -> 7 个求值点，纯组合逻辑，latency=0。
  * 输入布局 r(0..3) 是一个四项多项式的低到高系数。
  * 输出点顺序兼容旧 1024 版本：inf, 2, 1, -1, 1/2(缩放), -1/2(缩放), 0。
  */
class EvalLayerTC4(inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val r   = Input(Vec(4, UInt(inW.W)))
    val out = Output(Vec(7, UInt(outW.W)))
  })

  val even = io.r(0) +& io.r(2)
  val odd  = io.r(1) +& io.r(3)

  val scaledEven = (io.r(0) << 2) +& io.r(2)
  val scaledOdd  = (io.r(1) << 2) +& io.r(3)

  val high0 = io.r(2) +& (io.r(3) << 1)
  val high1 = io.r(1) +& (high0 << 1)
  val high2 = io.r(0) +& (high1 << 1)

  io.out(0) := CoreUtil.mask(io.r(3), outW)
  io.out(1) := CoreUtil.mask(high2, outW)
  io.out(2) := CoreUtil.mask(even +& odd, outW)
  io.out(3) := CoreUtil.fillMsb(even -& odd, outW)
  io.out(4) := CoreUtil.mask((scaledEven << 1) +& scaledOdd, outW)
  io.out(5) := CoreUtil.fillMsb((scaledEven << 1) -& scaledOdd, outW)
  io.out(6) := CoreUtil.mask(io.r(0), outW)
}

/** 单列 TC4 插值核心，纯组合逻辑，latency=0。
  * pIn 为 7 个点值；pr0/pr1/pr2 是前一列传来的高阶修正项。
  * 该模块先用整数 Lagrange 公式恢复 7 个局部系数 c0..c6，再沿用旧 Core16TC4
  * 的列间修正布局：输出 c3/c0part/c1part/c2part，并传递 nr0=c6,nr1=c1,nr2=c2。
  */
class InterpCoreTC4(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  private val p = InterpParamTable.params(stride, inW, outW)
  private val workW = p.mk + 24

  val io = IO(new Bundle {
    val pIn = Input(Vec(7, UInt(inW.W)))
    val pr0 = Input(UInt(outW.W))
    val pr1 = Input(UInt(outW.W))
    val pr2 = Input(UInt(outW.W))

    val c3     = Output(UInt(outW.W))
    val c0part = Output(UInt(outW.W))
    val c1part = Output(UInt(outW.W))
    val c2part = Output(UInt(outW.W))
    val nr0    = Output(UInt(outW.W))
    val nr1    = Output(UInt(outW.W))
    val nr2    = Output(UInt(outW.W))
  })

  require(p.pidx == pidx, s"pidx $pidx does not match stride $stride")

  def sx(u: UInt): SInt = CoreUtil.fillMsb(u, workW).asSInt
  def q(n: SInt, d: Int): SInt = CoreUtil.signedResize(n / d.S(workW.W), workW)
  def out(n: SInt): UInt = CoreUtil.signedToUInt(n, outW)

  val w0 = sx(io.pIn(0))
  val w1 = sx(io.pIn(1))
  val w2 = sx(io.pIn(2))
  val w3 = sx(io.pIn(3))
  val w4 = sx(io.pIn(4))
  val w5 = sx(io.pIn(5))
  val w6 = sx(io.pIn(6))

  def c(value: Int): SInt = value.S(workW.W)

  val c0 = w6
  val c1 = q(c(-90)*w0 + c(2)*w1 + c(-60)*w2 + c(20)*w3 + c(5)*w4 + c(-3)*w5 + c(-90)*w6, 180)
  val c2 = q(c(6)*w0 + c(-4)*w2 + c(-4)*w3 + w4 + w5 + c(-120)*w6, 24)
  val c3 = q(c(45)*w0 - w1 + c(27)*w2 + c(-7)*w3 - w4 + c(45)*w6, 18)
  val c4 = q(c(-30)*w0 + c(16)*w2 + c(16)*w3 - w4 - w5 + c(96)*w6, 24)
  val c5 = q(c(-360)*w0 + c(8)*w1 + c(-120)*w2 + c(-40)*w3 + c(5)*w4 + c(3)*w5 + c(-360)*w6, 180)
  val c6 = w0

  io.c3     := out(c3)
  io.c0part := CoreUtil.mask(out(c0) + io.pr2, outW)
  io.c1part := CoreUtil.mask(out(c5) + io.pr1, outW)
  io.c2part := CoreUtil.mask(out(c4) + io.pr0, outW)
  io.nr0    := out(c6)
  io.nr1    := out(c1)
  io.nr2    := out(c2)
}

/** TC4 插值层，纯组合逻辑，latency=0。
  * wIn 采用 point-major 布局：wIn(pt * stride + col)。
  * cOut 采用列展开布局：cOut(4 * col + k)。支持 stride=1/4/16。
  */
class InterpLayerTC4(stride: Int, pidx: Int, inW: Int, outW: Int) extends Module {
  val io = IO(new Bundle {
    val wIn  = Input(Vec(7 * stride, UInt(inW.W)))
    val cOut = Output(Vec(4 * stride, UInt(outW.W)))
  })

  val cRaw  = Wire(Vec(4 * stride, UInt(outW.W)))
  val prevR0 = Wire(Vec(stride + 1, UInt(outW.W)))
  val prevR1 = Wire(Vec(stride + 1, UInt(outW.W)))
  val prevR2 = Wire(Vec(stride + 1, UInt(outW.W)))

  prevR0(0) := 0.U
  prevR1(0) := 0.U
  prevR2(0) := 0.U

  for (col <- 0 until stride) {
    val core = Module(new InterpCoreTC4(stride, pidx, inW, outW))
    for (pt <- 0 until 7) core.io.pIn(pt) := io.wIn(pt * stride + col)
    core.io.pr0 := prevR0(col)
    core.io.pr1 := prevR1(col)
    core.io.pr2 := prevR2(col)

    cRaw(4 * col + 3) := core.io.c3
    cRaw(4 * col + 0) := core.io.c0part
    cRaw(4 * col + 1) := core.io.c1part
    cRaw(4 * col + 2) := core.io.c2part

    prevR0(col + 1) := core.io.nr0
    prevR1(col + 1) := core.io.nr1
    prevR2(col + 1) := core.io.nr2
  }

  for (i <- 0 until 4 * stride) io.cOut(i) := cRaw(i)
  io.cOut(0) := CoreUtil.mask(cRaw(0) - prevR2(stride), outW)
  io.cOut(1) := CoreUtil.mask(cRaw(1) - prevR1(stride), outW)
  io.cOut(2) := CoreUtil.mask(cRaw(2) - prevR0(stride), outW)
}

class CoreIO(params: CoreParams) extends Bundle {
  val valid_in  = Input(Bool())
  val avec      = Input(Vec(params.degree, UInt(params.aWidth.W)))
  val bvec      = Input(Vec(params.degree, UInt(params.bWidth.W)))
  val valid_out = Output(Bool())
  val cOut      = Output(Vec(params.degree, UInt(params.outWidth.W)))
}

/** degree=4 基础 Core。
  * 结构：EvalLayerTC4 -> 7 个有符号点乘 -> 可选 1 拍寄存器 -> InterpLayerTC4(stride=1)。
  * 默认 latency=1；关闭 pipelineProductToInterp 时 latency=0。
  */
private class BaseCore4(params: CoreParams) extends Module {
  require(params.degree == 4)
  val io = IO(new CoreIO(params))

  val evalA = Module(new EvalLayerTC4(params.aWidth, params.aEvalWidth))
  val evalB = Module(new EvalLayerTC4(params.bWidth, params.bEvalWidth))
  evalA.io.r := io.avec
  evalB.io.r := io.bvec

  val wMul = Wire(Vec(params.pointCount, UInt(params.productMulWidth.W)))
  for (pt <- 0 until params.pointCount) {
    val aw = evalA.io.out(pt).asSInt
    val bw = CoreUtil.fillMsb(evalB.io.out(pt), params.aEvalWidth max params.bEvalWidth).asSInt
    val awWide = CoreUtil.fillMsb(aw.asUInt, params.productMulWidth).asSInt
    val bwWide = CoreUtil.fillMsb(bw.asUInt, params.productMulWidth).asSInt
    wMul(pt) := CoreUtil.mask((awWide * bwWide).asUInt, params.productMulWidth)
  }

  val interpIn = if (params.pipelineProductToInterp) RegEnable(wMul, io.valid_in) else wMul
  val interpValid = if (params.pipelineProductToInterp) RegNext(io.valid_in, false.B) else io.valid_in

  val interp = Module(new InterpLayerTC4(
    stride = 1,
    pidx = params.interpPidx,
    inW = params.productMulWidth,
    outW = params.outWidth
  ))
  interp.io.wIn := interpIn

  io.valid_out := interpValid
  io.cOut := interp.io.cOut
}

/** 参数化递归 Toom-Cook-4 Core。
  * degree=4 使用 BaseCore4；degree=16 自动实例化 7 个 degree=4 child；
  * degree=64 自动实例化 7 个 degree=16 child。每个递归层在 child pointProducts
  * 到当前层 InterpLayerTC4 之间打一拍，默认总 latency = log4(degree)。
  */
class Core(params: CoreParams) extends Module {
  val io = IO(new CoreIO(params))

  if (params.degree == 4) {
    val base = Module(new BaseCore4(params))
    base.io.valid_in := io.valid_in
    base.io.avec := io.avec
    base.io.bvec := io.bvec
    io.valid_out := base.io.valid_out
    io.cOut := base.io.cOut
  } else {
    val childParams = params.child
    val segmentSize = params.segmentSize

    val childA = Wire(Vec(params.pointCount, Vec(segmentSize, UInt(childParams.aWidth.W))))
    val childB = Wire(Vec(params.pointCount, Vec(segmentSize, UInt(childParams.bWidth.W))))

    for (col <- 0 until segmentSize) {
      val evalA = Module(new EvalLayerTC4(params.aWidth, params.aEvalWidth))
      val evalB = Module(new EvalLayerTC4(params.bWidth, params.bEvalWidth))
      for (seg <- 0 until params.baseRadix) {
        evalA.io.r(seg) := io.avec(seg * segmentSize + col)
        evalB.io.r(seg) := io.bvec(seg * segmentSize + col)
      }
      for (pt <- 0 until params.pointCount) {
        childA(pt)(col) := evalA.io.out(pt)
        childB(pt)(col) := evalB.io.out(pt)
      }
    }

    val children = Seq.fill(params.pointCount)(Module(new Core(childParams)))
    for (pt <- 0 until params.pointCount) {
      children(pt).io.valid_in := io.valid_in
      children(pt).io.avec := childA(pt)
      children(pt).io.bvec := childB(pt)
    }

    val childValid = children.head.io.valid_out
    // 7 个 child 使用相同 params 和 valid_in，因此 valid_out 固定同步。

    val interpInComb = Wire(Vec(params.pointCount * segmentSize, UInt(childParams.outWidth.W)))
    for (pt <- 0 until params.pointCount) {
      for (col <- 0 until segmentSize) {
        interpInComb(pt * segmentSize + col) := children(pt).io.cOut(col)
      }
    }

    val interpIn = if (params.pipelineProductToInterp) RegEnable(interpInComb, childValid) else interpInComb
    val interpValid = if (params.pipelineProductToInterp) RegNext(childValid, false.B) else childValid

    val interp = Module(new InterpLayerTC4(
      stride = segmentSize,
      pidx = params.interpPidx,
      inW = childParams.outWidth,
      outW = params.outWidth
    ))
    interp.io.wIn := interpIn

    io.valid_out := interpValid
    io.cOut := interp.io.cOut
  }
}
