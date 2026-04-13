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

  // ---------- 状态机 ----------
  val sIdle :: sCompute :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val col = RegInit(0.U(2.W)) // 当前列 i = 0..3

  // ---------- 寄存器：跨列保存 r[7/8/9]（即上一列的 r[0/1/2]） ----------
  val prev0 = RegInit(0.U(27.W)) // r[7] = 上列 r[0]
  val prev1 = RegInit(0.U(27.W)) // r[8] = 上列 r[1]
  val prev2 = RegInit(0.U(27.W)) // r[9] = 上列 r[2]

  // ---------- 输出寄存器 ----------
  val cReg = RegInit(VecInit(Seq.fill(16)(0.U(27.W))))

  // ---------- 状态跳转 ----------
  switch(state) {
    is(sIdle) {
      when(io.valid_in) {
        state := sCompute
        col := 0.U
        prev0 := 0.U
        prev1 := 0.U
        prev2 := 0.U
      }
    }
    is(sCompute) {
      when(col === 3.U) { state := sDone }
        .otherwise { col := col + 1.U }
    }
    is(sDone) {
      when(io.valid_in) {
        state := sCompute
        col := 0.U
        prev0 := 0.U
        prev1 := 0.U
        prev2 := 0.U
      }
        .otherwise { state := sIdle }
    }
  }

  // ---------- 组合逻辑 ----------
  val r0_in = io.w(0.U * 4.U + col)
  val r1_in = io.w(1.U * 4.U + col)
  val r2_in = io.w(2.U * 4.U + col)
  val r3_in = io.w(3.U * 4.U + col)
  val r4_in = io.w(4.U * 4.U + col)
  val r5_in = io.w(5.U * 4.U + col)
  val r6_in = io.w(6.U * 4.U + col)

  val MASK = "h7FFFFFF".U(27.W)

  val s_r0 = r0_in
  val s_r1_a = (r1_in + r4_in) & MASK
  val s_r2 = r2_in
  val s_r3_a = ((r3_in - r2_in) >> 1) & MASK
  val s_r4_a = r4_in
  val s_r5_a = (r5_in - r4_in) & MASK
  val s_r6 = r6_in

  val s_r4_b = ((((s_r4_a - s_r0) << 1) + s_r5_a) - (s_r6 << 7)) & MASK
  val s_r2_b = (s_r2 + s_r3_a) & MASK
  val s_r1_b = (s_r1_a - (s_r2_b << 6) - s_r2_b) & MASK

  val s_r2_c = (s_r2_b - s_r6 - s_r0) & MASK
  val s_r1_c =
    (s_r1_b + s_r2_c + (s_r2_c << 2) + (s_r2_c << 3) + (s_r2_c << 5)) & MASK

  val s_r4_c = (((s_r4_b - (s_r2_c << 3)) >> 3) & MASK) * "hAAAAAAB".U(28.W)
  val s_r4_d = (s_r4_c >> 0) & MASK
  val s_r5_b = (s_r5_a + s_r1_c) & MASK

  val s_r1_d = (((s_r1_c + (s_r3_a << 4)) >> 1) & MASK) * "h8E38E39".U(28.W)
  val s_r1_e = (s_r1_d >> 0) & MASK
  val s_r3_b = (-s_r3_a - s_r1_e) & MASK

  val s_r5_c_inner = ((s_r5_b >> 1) * "hEEEEEEF".U(28.W)) & MASK
  val s_r5_d = ((s_r1_e - s_r5_c_inner) >> 1) & MASK
  val s_r2_d = (s_r2_c - s_r4_d) & MASK

  val s_r1_f = (s_r1_e - s_r5_d) & MASK

  // ---------- 将本列结果写入 cReg ----------
  when(state === sCompute) {
    cReg(col * 4.U + 3.U) := s_r3_b

    when(col === 0.U) {
      cReg(0.U) := s_r6
      cReg(1.U) := s_r5_d
      cReg(2.U) := s_r4_d
    }.otherwise {
      cReg(col * 4.U + 0.U) := (s_r6 + prev2) & MASK // r[6] + r[9]
      cReg(col * 4.U + 1.U) := (s_r5_d + prev1) & MASK // r[5] + r[8]
      cReg(col * 4.U + 2.U) := (s_r4_d + prev0) & MASK // r[4] + r[7]
    }

    // 保存本列 r[0/1/2] 供下列使用
    prev0 := s_r0
    prev1 := s_r1_f
    prev2 := s_r2_d

    // 最后一列：对 c[0..2] 做最终修正
    when(col === 3.U) {
      cReg(0.U) := (cReg(0.U) - s_r2_d) & MASK
      cReg(1.U) := (cReg(1.U) - s_r1_f) & MASK
      cReg(2.U) := (cReg(2.U) - s_r0) & MASK
    }
  }

  // ---------- 输出 ----------
  io.valid_out := (state === sDone)
  for (i <- 0 until 16) {
    io.c(i) := cReg(i)(23, 0)
  }
}
