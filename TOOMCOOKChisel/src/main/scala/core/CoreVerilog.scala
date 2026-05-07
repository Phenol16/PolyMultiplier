package core
import chisel3._
import chisel3.util._

object CoreVerilog extends App {
  private def arg(name: String, default: Int): Int = {
    args.sliding(2, 1).collectFirst { case Array(k, v) if k == s"--$name" => v.toInt }.getOrElse(default)
  }
  val params = CoreParams(
    degree = arg("degree", 16),
    aWidth = arg("aWidth", 24),
    bWidth = arg("bWidth", 8)
  )
  emitVerilog(new Core(params), Array("--target-dir", s"generated/Core_d${params.degree}_a${params.aWidth}_b${params.bWidth}"))
}

object EmitCore4 extends App { emitVerilog(new Core(CoreParams(4, 24, 8)), Array("--target-dir", "generated/Core4")) }
object EmitCore16 extends App { emitVerilog(new Core(CoreParams(16, 24, 8)), Array("--target-dir", "generated/Core16")) }
object EmitCore64 extends App { emitVerilog(new Core(CoreParams(64, 24, 8)), Array("--target-dir", "generated/Core64")) }
