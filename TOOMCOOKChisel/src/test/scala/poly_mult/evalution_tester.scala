package poly_mult
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class EvalTest extends AnyFlatSpec with ChiselScalatestTester {
  "evaluation" should "work" in {
    test(new evaluation) { c =>

  }
}
