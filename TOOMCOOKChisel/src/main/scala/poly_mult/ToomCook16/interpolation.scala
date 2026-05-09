package poly_mult
import chisel3._
import chisel3.util._

class interpIO extends Bundle {
  val valid_in = Input(Bool())
  val w = Input(Vec(28, UInt(27.W)))
  val valid_out = Output(Bool())
  val c = Output(Vec(16, UInt(24.W)))
}

class interpolation extends Module {
  val io = IO(new interpIO)

  val MASK27 = "h7FFFFFF".U
  val MASK25 = "h1FFFFFF".U
  val MASK24 = "hFFFFFF".U

  // ---- col 0 --------------------------------------------------------
  val w00 = io.w(0); val w10 = io.w(4)
  val w20 = io.w(8); val w30 = io.w(12)
  val w40 = io.w(16); val w50 = io.w(20)
  val w60 = io.w(24)

  val c0_r1a = (w10 + w40) & MASK27
  val c0_r5a = (w50 - w40) & MASK27
  val c0_r3a = ((w30 - w20) >> 1) & MASK27
  val c0_r4a = (w40 - w00) & MASK27
  val c0_r4b = ((c0_r4a << 1) + c0_r5a - (w60 << 7)) & MASK27
  val c0_r2a = (w20 + c0_r3a) & MASK27
  val c0_r1b = (c0_r1a - (c0_r2a << 6) - c0_r2a) & MASK27
  val c0_r2b = (c0_r2a - w60 - w00) & MASK27
  val c0_r1c = (c0_r1b + c0_r2b + (c0_r2b << 2) +
    (c0_r2b << 3) + (c0_r2b << 5)) & MASK27

  val c0_r4d = (((c0_r4b - (c0_r2b << 3)) & MASK27) >> 3) *
    "hAAAAAAB".U & MASK27
  val c0_r5c = (((c0_r5a + c0_r1c) >> 1) *
    "hEEEEEF".U) & MASK25
  val c0_r1e = ((((c0_r1c + (c0_r3a << 4)) & MASK27) >> 1) *
    "hE38E39".U) & MASK25

  val c0_r2c = (c0_r2b - c0_r4d) & MASK24
  val c0_r3b = (-c0_r1e - c0_r3a) & MASK24
  val c0_r5d = (c0_r1e - c0_r5c) >> 1
  val c0_r1f = c0_r1e - c0_r5d

  // ---- col 1 --------------------------------------------------------
  val w01 = io.w(1); val w11 = io.w(5)
  val w21 = io.w(9); val w31 = io.w(13)
  val w41 = io.w(17); val w51 = io.w(21)
  val w61 = io.w(25)

  val c1_r1a = (w11 + w41) & MASK27
  val c1_r5a = (w51 - w41) & MASK27
  val c1_r3a = ((w31 - w21) >> 1) & MASK27
  val c1_r4a = (w41 - w01) & MASK27
  val c1_r4b = ((c1_r4a << 1) + c1_r5a - (w61 << 7)) & MASK27
  val c1_r2a = (w21 + c1_r3a) & MASK27
  val c1_r1b = (c1_r1a - (c1_r2a << 6) - c1_r2a) & MASK27
  val c1_r2b = (c1_r2a - w61 - w01) & MASK27
  val c1_r1c = (c1_r1b + c1_r2b + (c1_r2b << 2) +
    (c1_r2b << 3) + (c1_r2b << 5)) & MASK27

  val c1_r4d = (((c1_r4b - (c1_r2b << 3)) & MASK27) >> 3) *
    "hAAAAAAB".U & MASK27
  val c1_r5c = (((c1_r5a + c1_r1c) >> 1) *
    "hEEEEEF".U) & MASK25
  val c1_r1e = ((((c1_r1c + (c1_r3a << 4)) & MASK27) >> 1) *
    "hE38E39".U) & MASK25

  val c1_r2c = (c1_r2b - c1_r4d) & MASK24
  val c1_r3b = (-c1_r1e - c1_r3a) & MASK24
  val c1_r5d = (c1_r1e - c1_r5c) >> 1
  val c1_r1f = c1_r1e - c1_r5d

  // ---- col 2 --------------------------------------------------------
  val w02 = io.w(2); val w12 = io.w(6)
  val w22 = io.w(10); val w32 = io.w(14)
  val w42 = io.w(18); val w52 = io.w(22)
  val w62 = io.w(26)

  val c2_r1a = (w12 + w42) & MASK27
  val c2_r5a = (w52 - w42) & MASK27
  val c2_r3a = ((w32 - w22) >> 1) & MASK27
  val c2_r4a = (w42 - w02) & MASK27
  val c2_r4b = ((c2_r4a << 1) + c2_r5a - (w62 << 7)) & MASK27
  val c2_r2a = (w22 + c2_r3a) & MASK27
  val c2_r1b = (c2_r1a - (c2_r2a << 6) - c2_r2a) & MASK27
  val c2_r2b = (c2_r2a - w62 - w02) & MASK27
  val c2_r1c = (c2_r1b + c2_r2b + (c2_r2b << 2) +
    (c2_r2b << 3) + (c2_r2b << 5)) & MASK27

  val c2_r4d = (((c2_r4b - (c2_r2b << 3)) & MASK27) >> 3) *
    "hAAAAAAB".U & MASK27
  val c2_r5c = (((c2_r5a + c2_r1c) >> 1) *
    "hEEEEEF".U) & MASK25
  val c2_r1e = ((((c2_r1c + (c2_r3a << 4)) & MASK27) >> 1) *
    "hE38E39".U) & MASK25

  val c2_r2c = (c2_r2b - c2_r4d) & MASK24
  val c2_r3b = (-c2_r1e - c2_r3a) & MASK24
  val c2_r5d = (c2_r1e - c2_r5c) >> 1
  val c2_r1f = c2_r1e - c2_r5d

  // ---- col 3 --------------------------------------------------------
  val w03 = io.w(3); val w13 = io.w(7)
  val w23 = io.w(11); val w33 = io.w(15)
  val w43 = io.w(19); val w53 = io.w(23)
  val w63 = io.w(27)

  val c3_r1a = (w13 + w43) & MASK27
  val c3_r5a = (w53 - w43) & MASK27
  val c3_r3a = ((w33 - w23) >> 1) & MASK27
  val c3_r4a = (w43 - w03) & MASK27
  val c3_r4b = ((c3_r4a << 1) + c3_r5a - (w63 << 7)) & MASK27
  val c3_r2a = (w23 + c3_r3a) & MASK27
  val c3_r1b = (c3_r1a - (c3_r2a << 6) - c3_r2a) & MASK27
  val c3_r2b = (c3_r2a - w63 - w03) & MASK27
  val c3_r1c = (c3_r1b + c3_r2b + (c3_r2b << 2) +
    (c3_r2b << 3) + (c3_r2b << 5)) & MASK27

  val c3_r4d = (((c3_r4b - (c3_r2b << 3)) & MASK27) >> 3) *
    "hAAAAAAB".U & MASK27
  val c3_r5c = (((c3_r5a + c3_r1c) >> 1) *
    "hEEEEEF".U) & MASK25
  val c3_r1e = ((((c3_r1c + (c3_r3a << 4)) & MASK27) >> 1) *
    "hE38E39".U) & MASK25

  val c3_r2c = (c3_r2b - c3_r4d) & MASK24
  val c3_r3b = (-c3_r1e - c3_r3a) & MASK24
  val c3_r5d = (c3_r1e - c3_r5c) >> 1
  val c3_r1f = c3_r1e - c3_r5d

  // ------------------------------------------------------------------
  // 拼装 c_wire[0..15]
  // ------------------------------------------------------------------
  val cWire = Wire(Vec(16, UInt(27.W)))

  cWire(0) := (w60 - c3_r2c) & MASK24 // c[0]=r6 - r2(final)
  cWire(1) := (c0_r5d - c3_r1f) & MASK24 // c[1]=r5 - r1(final)
  cWire(2) := (c0_r4d - w03) & MASK24 // c[2]=r4 - r0(final)
  cWire(3) := c0_r3b

  cWire(4) := (w61 + c0_r2c) & MASK24
  cWire(5) := (c1_r5d + c0_r1f) & MASK24
  cWire(6) := (c1_r4d + w00) & MASK24
  cWire(7) := c1_r3b

  cWire(8) := (w62 + c1_r2c) & MASK24
  cWire(9) := (c2_r5d + c1_r1f) & MASK24
  cWire(10) := (c2_r4d + w01) & MASK24
  cWire(11) := c2_r3b

  cWire(12) := (w63 + c2_r2c) & MASK24
  cWire(13) := (c3_r5d + c2_r1f) & MASK24
  cWire(14) := (c3_r4d + w02) & MASK24
  cWire(15) := c3_r3b

  // ------------------------------------------------------------------
  // 只打一拍
  // ------------------------------------------------------------------
  val validReg = RegNext(io.valid_in, false.B)
  val cReg = RegNext(cWire)

  io.valid_out := validReg
  for (i <- 0 until 16) {
    io.c(i) := cReg(i)(23, 0)
  }
}
