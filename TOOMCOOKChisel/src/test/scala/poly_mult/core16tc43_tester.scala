package poly_mult

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Core16TC43Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Core16TC43"

  it should "elaborate with top-eval widths (33,20)" in {
    test(new Core16TC43) { _ =>
      succeed
    }
  }
}
