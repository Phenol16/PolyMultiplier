package core
import chisel3._
import chisel3.util._

/** Core 参数集中管理。
  *
  * - degree 只允许 4/16/64，递归层数 level=log4(degree)。
  * - 外部入口只允许题目指定的 aWidth/bWidth；递归 child 会使用 eval 后位宽，
  *   因此通过 internal=true 放宽输入位宽集合检查。
  * - 位宽采用保守增长规则：TC4 求值最大包含 8*x，另留 1 bit 符号余量，
  *   所以每层 evalWidth = inWidth + 4。乘法点值完整保留 aEval+bEval+1 位。
  *   每层 coreOutWidth 保持为 aWidth+bWidth+8*level+8；递归 child 的输入各增加 4 bit，
  *   level 减 1，得到相同 outWidth，便于 InterpLayer 分层连接且避免截断不足。
  */
case class CoreParams(
  degree: Int,
  aWidth: Int,
  bWidth: Int,
  pipelineProductToInterp: Boolean = true,
  internal: Boolean = false
) {
  require(Set(4, 16, 64).contains(degree), s"degree must be one of 4/16/64, got $degree")
  if (!internal) {
    require(Set(24, 28, 32, 36).contains(aWidth), s"aWidth must be one of 24/28/32/36, got $aWidth")
    require(Set(8, 10, 12, 14, 16).contains(bWidth), s"bWidth must be one of 8/10/12/14/16, got $bWidth")
  }
  require(aWidth > 0 && bWidth > 0, "widths must be positive")

  val pointCount: Int = 7
  val baseRadix: Int = 4
  val level: Int = degree match {
    case 4  => 1
    case 16 => 2
    case 64 => 3
  }
  val segmentSize: Int = degree / baseRadix

  val aEvalWidth: Int = aWidth + 4
  val bEvalWidth: Int = bWidth + 4
  val productMulWidth: Int = aEvalWidth + bEvalWidth + 1
  val productOutWidth: Int = aWidth + bWidth + 8 * level + 8
  val coreOutWidth: Int = productOutWidth
  val outWidth: Int = coreOutWidth

  val interpStride: Int = segmentSize
  val interpPidx: Int = InterpParamTable.pidxForStride(interpStride)
  val expectedLatency: Int = if (pipelineProductToInterp) level else 0

  def child: CoreParams = {
    require(degree > 4, "degree=4 has no recursive child")
    copy(
      degree = segmentSize,
      aWidth = aEvalWidth,
      bWidth = bEvalWidth,
      internal = true
    )
  }
}
