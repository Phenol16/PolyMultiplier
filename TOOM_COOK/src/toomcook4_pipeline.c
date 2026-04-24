/*
 * toomcook4_pipeline.c
 *
 * 硬件行为模型：Striding Toom-Cook 4路，三级流水，无递归
 *
 * 评估核接口（固定）：
 *   输入  : a[0], a[stride], a[2*stride], a[3*stride]  （4个系数，stride采样）
 *   输出  : 7个评估点，每点写回 out[k*stride + j]，k=0..6
 *   即：16个有效输入数（4系数×N/4块），28个有效输出数（7点×4系数）
 *
 * 数据布局（始终保持 striding 格式）：
 *   buf[point * stride + coeff_index]
 *   其中 stride = N/4，point = 0..6
 *
 * 三级流水：
 *   ┌─────────────────────────────────────────────────────┐
 *   │  EVAL STAGE                                         │
 *   │  L0: stride=256, 运行256次评估核 → aws0[7*256]      │
 *   │  L1: stride=64,  运行256次评估核 × 7组 → aws1[49*64]│  (每组64次)
 *   │  L2: stride=16,  运行64次评估核 × 49组 → aws2[343*16]│ (每组16次)  [实际子块4系数]
 *   ├─────────────────────────────────────────────────────┤
 *   │  POINTWISE MUL: 343 × product16()                  │
 *   ├─────────────────────────────────────────────────────┤
 *   │  INTERP STAGE (逆序)                                │
 *   │  L2: 运行16次插值核 × 49组 → w1[49*64]              │
 *   │  L1: 运行64次插值核 × 7组  → w0[7*256]              │
 *   │  L0: 运行256次插值核       → c[1024]                │
 *   └─────────────────────────────────────────────────────┘
 */

 #include <stdint.h>
 #include <string.h>
 
 /* ------------------------------------------------------------------ */
 /*  掩码                                                                */
 /* ------------------------------------------------------------------ */
 #define mask39  0x7FFFFFFFFFULL
 #define mask37  0x1FFFFFFFFFULL
 #define mask36  0xFFFFFFFFFULL
 #define mask34  0x3FFFFFFFFULL
 #define mask33  0x1FFFFFFFFULL
 #define mask31  0x7FFFFFFFULL
 #define mask30  0x3FFFFFFFULL
 #define mask28  0xFFFFFFFULL
 #define mask27  0x7FFFFFFULL
 #define mask25  0x1FFFFFFULL
 #define mask24  0xFFFFFFULL
 
 /* ================================================================== */
 /*  评估核：固定接口，16输入 → 28输出                                   */
 /*                                                                      */
 /*  对 buf 中第 j 号块做评估，按 stride 步长采样4个系数，               */
 /*  结果写回 7 个评估点位置。                                           */
 /*                                                                      */
 /*  buf[k * stride + j]  k=0..6 为7个评估点（输出）                    */
 /*  buf[k * stride + j]  在调用前 k=0..3 存放输入系数 r0..r3           */
 /*  调用后 k=0..6 全部被覆盖为评估结果                                  */
 /*                                                                      */
 /*  参数：                                                               */
 /*    buf    : 同一块内存，in/out 复用                                   */
 /*    j      : 当前块索引 (0 .. stride-1)                               */
 /*    stride : N/4，当前层的子块数量                                     */
 /* ================================================================== */
 static inline void eval_core(uint64_t *buf, int j, int stride)
 {
     /* 读取4个输入系数（stride采样） */
     uint64_t r0 = buf[0 * stride + j];
     uint64_t r1 = buf[1 * stride + j];
     uint64_t r2 = buf[2 * stride + j];
     uint64_t r3 = buf[3 * stride + j];
 
     uint64_t t0, t1;
 
     /* 评估点 p(∞) = r3 */
     buf[0 * stride + j] = r3;
 
     /* 评估点 p(1) = r3+r2+r1+r0 */
     buf[1 * stride + j] = (r3 << 3) + (r2 << 2) + (r1 << 1) + r0;
 
     /* 评估点 p(1) = r0+r1+r2+r3（实际是 p(1) = sum） */
     t0 = r0 + r2;
     t1 = r1 + r3;
     buf[2 * stride + j] = t0 + t1;   /* p(1)  */
     buf[3 * stride + j] = t0 - t1;   /* p(-1) */
 
     /* 评估点 p(2), p(-2) */
     t0 = (((r0 << 2) + r2) << 1);
     t1 = (r1 << 2) + r3;
     buf[4 * stride + j] = t0 + t1;   /* p(2)  */
     buf[5 * stride + j] = t0 - t1;   /* p(-2) */
 
     /* 评估点 p(0) = r0 */
     buf[6 * stride + j] = r0;
 }
 
 /* ================================================================== */
 /*  插值核：固定接口，对第 i 号位置做插值                               */
 /*                                                                      */
 /*  从 w[k * stride + i]（k=0..6）读取7个评估点结果，                  */
 /*  计算插值系数 r0..r6，写入输出数组 c（重叠加法方式）。               */
 /*                                                                      */
 /*  参数：                                                               */
 /*    w      : 7×stride 的评估点数据                                     */
 /*    c      : 输出多项式（长度 4*stride），重叠加法方式累积              */
 /*    i      : 当前位置索引 (0 .. stride-1)                             */
 /*    stride : 当前层 N/4                                               */
 /*    prev   : 上一次调用留下的 r[0..2]（用于重叠加法），首次调用传NULL  */
 /*    N      : 当前层级大小（= stride * 4，用于选掩码）                  */
 /* ================================================================== */
 
 /* 插值参数表，按 N（=stride*4）索引 */
 typedef struct {
     uint64_t mk;      /* 主掩码（大多数步骤用） */
     uint64_t mk_r2;   /* r2 输出掩码 */
     uint64_t mk_r13;  /* r1/r3 输出掩码 */
     uint64_t inv3;    /* 乘以 1/3 的魔数 */
     uint64_t inv9;    /* 乘以 1/9 的魔数 */
     uint64_t inv18;   /* 乘以 1/18 的魔数 */
 } InterpParam;
 
 static const InterpParam INTERP_PARAMS[] = {
     /* N=4   */ { mask36, mask33, mask34, 0xAAAAAAABULL,  0x238E38E39ULL, 0x2EEEEEEEFULL },
     /* N=16  */ { mask33, mask30, mask31, 0x2AAAAAABULL,  0x38E38E39ULL,  0x6EEEEEEFULL  },
     /* N=64  */ { mask30, mask27, mask28, 0x2AAAAABULL,   0x8E38E39ULL,   0xEEEEEEFULL   },
     /* N=256 */ { mask27, mask24, mask25, 0xAAAAABULL,    0xE38E39ULL,    0xEEEEEFULL    },
 };
 
 /* 根据 N 获取参数表索引 */
 static inline int interp_param_idx(int N)
 {
     switch (N) {
         case   4: return 0;
         case  16: return 1;
         case  64: return 2;
         case 256: return 3;
         default:  return -1;
     }
 }
 
 /*
  * interp_core：
  *   对单个位置 i 执行插值，返回本次的 r0/r1/r2 供下一次重叠加法使用。
  *
  *   out_r0/1/2 : [out] 本次产生的高位系数，下一块需要加到 c[4*(i+1)+0/1/2]
  *   prev_r0/1/2: [in]  上一块产生的高位系数，叠加到 c[4*i+0/1/2]
  */
 static inline void interp_core(
     const uint64_t *w, uint64_t *c,
     int i, int stride,
     const InterpParam *p,
     uint64_t prev_r0, uint64_t prev_r1, uint64_t prev_r2,
     uint64_t *out_r0, uint64_t *out_r1, uint64_t *out_r2)
 {
     uint64_t p0 = w[0 * stride + i];
     uint64_t p1 = w[1 * stride + i];
     uint64_t p2 = w[2 * stride + i];
     uint64_t p3 = w[3 * stride + i];
     uint64_t p4 = w[4 * stride + i];
     uint64_t p5 = w[5 * stride + i];
     uint64_t p6 = w[6 * stride + i];
 
     uint64_t mk   = p->mk;
     uint64_t mk2  = p->mk_r2;
     uint64_t mk3  = p->mk_r13;
 
     uint64_t r1, r2, r3, r4, r5, r6;
 
     r5 = (p5 - p4) & mk;
     r3 = ((p3 - p2) & mk) >> 1;
     r4 = (p4 - p0) & mk;
 
     r4 = ((r4 << 1) + r5 - (p6 << 7)) & mk;
     r2 = (p2 + r3) & mk;
 
     r1 = (p1 + p4 - (r2 << 6) - r2) & mk;
     r2 = (r2 - p6 - p0) & mk;
 
     r1 = (r1 + r2 + (r2 << 2) + (r2 << 3) + (r2 << 5)) & mk;
     r4 = ((uint64_t)(((r4 - (r2 << 3)) & mk) >> 3) * p->inv3) & mk2;
 
     r5 = (((r5 + r1) >> 1) * p->inv18) & mk3;
     r1 = ((uint64_t)(((r1 + (r3 << 4)) & mk) >> 1) * p->inv9) & mk3;
     r2 = (r2 - r4) & mk2;
 
     r3 = (-r3 - r1) & mk2;
     r5 = (r1 - r5) >> 1;
     r1 = r1 - r5;
 
     r6 = p6;   /* 常数项直通（p(0) = r0_poly） */
 
     /* 输出：重叠加法 */
     c[4 * i + 3] = r3;
     c[4 * i + 0] = r6 + prev_r2;   /* 首次 i=0 时 prev_r2=0，正确 */
     c[4 * i + 1] = r5 + prev_r1;
     c[4 * i + 2] = r4 + prev_r0;
 
     /* 传出本次高位系数给下一块 */
     *out_r0 = p0;   /* r0_poly = p(∞) 系数，递推给下一块 */
     *out_r1 = r1;
     *out_r2 = r2;
 }
 
 /* ================================================================== */
 /*  整层评估：对长度 seg_len 的段，连续运行 (seg_len/4) 次评估核        */
 /*                                                                      */
 /*  in/out 布局：                                                        */
 /*    输入  in[k * stride + j]，k=0..3，j=0..stride-1                  */
 /*    输出 out[k * stride + j]，k=0..6，j=0..stride-1                  */
 /*    （stride = seg_len/4）                                             */
 /*                                                                      */
 /*  in 和 out 可以是同一块内存（in先被读取，再被覆盖）。               */
 /* ================================================================== */
 static void eval_layer(const uint64_t *in, uint64_t *out, int seg_len)
 {
     int stride = seg_len / 4;
 
     /* 先把 in 的低4行复制到 out（若 in != out） */
     if (in != out)
         memcpy(out, in, 4 * stride * sizeof(uint64_t));
     /* 高3行（k=4,5,6）由评估核填写，无需初始化 */
 
     /* 对每个块 j 运行评估核 */
     for (int j = 0; j < stride; j++)
         eval_core(out, j, stride);
 }
 
 /* ================================================================== */
 /*  整层插值：对 stride 个位置运行插值核，结果写入 c[0..4*stride-1]    */
 /* ================================================================== */
 static void interp_layer(const uint64_t *w, uint64_t *c, int stride)
 {
     int N = stride * 4;   /* 当前层多项式长度 */
     int idx = interp_param_idx(N);
     const InterpParam *p = &INTERP_PARAMS[idx];
 
     uint64_t pr0 = 0, pr1 = 0, pr2 = 0;   /* 上一块遗留的高位系数 */
     uint64_t nr0, nr1, nr2;
 
     for (int i = 0; i < stride; i++)
     {
         interp_core(w, c, i, stride, p,
                     pr0, pr1, pr2,
                     &nr0, &nr1, &nr2);
         pr0 = nr0; pr1 = nr1; pr2 = nr2;
     }
 
     /* 修正最后三个重叠项（等价于原始代码的 c[0]-=r[2] 等） */
     c[0] -= pr2;
     c[1] -= pr1;
     c[2] -= pr0;
 }
 
 /* ================================================================== */
 /*  16阶底层乘法核（product16）                                         */
 /*                                                                      */
 /*  输入：a[4], b[4]（4个系数）                                          */
 /*  输出：c[7]（product 后7个系数，供上层插值读取）                      */
 /*                                                                      */
 /*  内部使用评估核 + 逐点乘法 + 插值，完全等价于原始 product()。        */
 /* ================================================================== */
 static void product16(const uint64_t *a, const uint64_t *b, uint64_t *c)
 {
     /* 评估：将 a/b 的4个系数展开到7个评估点（stride=1） */
     uint64_t aws[7], bws[7];
     memcpy(aws, a, 4 * sizeof(uint64_t));
     memcpy(bws, b, 4 * sizeof(uint64_t));
     eval_core(aws, 0, 1);
     eval_core(bws, 0, 1);
 
     /* 逐点乘法（带符号位扩展，与原始代码一致） */
     uint64_t w[7];
     for (int i = 0; i < 7; i++)
     {
         uint64_t aw      = aws[i] & mask39;
         uint64_t bw      = bws[i] & 0x1FFFFFFFULL;
         uint64_t bw_sign = bw & 0x10000000ULL;
         int64_t  bw_s    = (int64_t)((bw | (uint64_t)(-(int64_t)bw_sign)) & mask39);
         w[i] = (uint64_t)((int64_t)aw * bw_s) & mask39;
     }
 
     /* 插值（N=4，stride=1）*/
     const InterpParam *p = &INTERP_PARAMS[0];   /* N=4 */
     uint64_t r1, r2, r3, r4, r5;
 
     r5 = (w[5] - w[4]) & mask39;
     r3 = ((w[3] - w[2]) & mask39) >> 1;
     r4 = (w[4] - w[0]) & mask39;
 
     r4 = ((r4 << 1) + r5 - (w[6] << 7)) & mask39;
     r2 = (w[2] + r3) & mask39;
 
     r1 = (w[1] + w[4] - (r2 << 6) - r2) & mask39;
     r2 = (r2 - w[6] - w[0]) & mask39;
 
     r1 = (r1 + r2 + (r2 << 2) + (r2 << 3) + (r2 << 5)) & mask39;
     r4 = ((uint64_t)(((r4 - (r2 << 3)) & mask39) >> 3) * 0xAAAAAAAABULL) & mask36;
 
     r5 = (((r5 + r1) >> 1) * 0xEEEEEEEEFULL) & mask37;
     r1 = ((uint64_t)(((r1 + (r3 << 4)) & mask39) >> 1) * 0xE38E38E39ULL) & mask37;
     r2 = (r2 - r4) & mask36;
 
     r3 = (-r3 - r1) & mask36;
     r5 = (r1 - r5) >> 1;
     r1 = r1 - r5;
 
     /*
      * 输出7个系数，对应 w[k*1+0]，k=0..6，供上层插值（N=16 的 interp_layer）
      * 按照 striding 布局，上层 interp_layer 读取 w[k*stride + i]。
      * product16 的输出直接是4个系数（c[0..3]），另外3个为0（最高次为0）。
      *
      * 注：上层 interp_layer(N=16) 将从 w 数组按 stride=4 读取，
      *     即读 w[k*4+i]，k=0..6。我们把7个系数打包进 c[0..6]。
      */
     c[0] = (w[6] - r2) & mask36;   /* 常数项 */
     c[1] = (r5 - r1)   & mask36;
     c[2] = (r4 - w[0]) & mask36;
     c[3] = r3          & mask36;
     c[4] = 0;
     c[5] = 0;
     c[6] = 0;
 }
 
 /* ================================================================== */
 /*  主函数：三级流水 Toom-Cook 1024 阶乘法                              */
 /* ================================================================== */
 void toomcook4_pipeline(const uint64_t *a, const uint64_t *b, uint64_t *c)
 {
     /* ============================================================
      * STAGE 1：三级评估
      *
      * 数据布局说明（striding）：
      *   第 L 级评估后，数据存在 [7^L × (1024/4^L)] 的数组中，
      *   其中每组内按 [point * (1024/4^(L+1)) + coeff] 排列。
      *
      * L=0：aws0[7  × 256]，stride=256
      * L=1：aws1[49 × 64]， stride=64
      * L=2：aws2[343× 16]， stride=16（最后4个是实际系数）
      * ============================================================ */
 
     /* --- L0 评估：输入 a[1024]，输出 aws0[7×256] --- */
     /*
      * 布局：aws0[k * 256 + j]，k=0..6 为评估点，j=0..255 为系数索引
      * eval_layer 对 j=0..255 每个位置调用 eval_core(stride=256)
      */
     uint64_t aws0[7 * 256], bws0[7 * 256];
     eval_layer(a, aws0, 1024);
     eval_layer(b, bws0, 1024);
 
     /*
      * --- L1 评估：对 aws0 中7个评估点各自的256系数段再做评估 ---
      *
      * aws0 的布局是 [point0 * 256 + coeff]。
      * 对于 point0=i，其256个系数为 aws0[i*256 + 0..255]。
      * 将这256个系数再做评估（stride=64），得 7×64 的输出。
      * 结果存入 aws1[i * 7 * 64 + point1 * 64 + coeff]。
      *
      * 等价于：aws1[(i*7+j)*64 + k]
      *   i = L0 评估点 (0..6)
      *   j = L1 评估点 (0..6)
      *   k = 系数索引 (0..63)
      */
     uint64_t aws1[7 * 7 * 64], bws1[7 * 7 * 64];
     for (int i = 0; i < 7; i++)
     {
         eval_layer(&aws0[i * 256], &aws1[i * 7 * 64], 256);
         eval_layer(&bws0[i * 256], &bws1[i * 7 * 64], 256);
     }
 
     /*
      * --- L2 评估：对 aws1 中 49 个段各自的64系数再做评估 ---
      *
      * 结果 aws2[(i*7+j)*7*16 + k*16 + coeff]
      *   i = L0 评估点, j = L1 评估点, k = L2 评估点
      *   coeff = 0..15（每段4个有效系数 + 7个评估点槽）
      *
      * 即 aws2[seg343 * 7 * 16 + point2 * 16 + c]
      *   seg343 = i*49+j*7+... 实际下面用 (i*7+j) 作 L1 段索引
      */
     uint64_t aws2[7 * 7 * 7 * 16], bws2[7 * 7 * 7 * 16];
     for (int i = 0; i < 7; i++)
     for (int j = 0; j < 7; j++)
     {
         int seg49 = i * 7 + j;
         eval_layer(&aws1[seg49 * 64], &aws2[seg49 * 7 * 16], 64);
         eval_layer(&bws1[seg49 * 64], &bws2[seg49 * 7 * 16], 64);
     }
 
     /*
      * 至此 aws2/bws2 的布局为：
      *   aws2[seg343 * 16 + coeff]，seg343 = 0..342，coeff = 0..15
      *   其中 coeff=0..3 是4个多项式系数（L2评估后每子块4系数）
      *
      * 等价理解：aws2 中有 343 组，每组是一个4系数的小多项式（N=16段）。
      * 相邻的7组（同一 seg49）按 stride=4 组织，eval_core 已按此排列。
      */
 
     /* ============================================================
      * STAGE 2：343 次 product16（时间复用同一乘法核）
      *
      * 对每个 seg343，取其4个系数做 product16，输出7个系数。
      * 结果存入 w2，布局与 aws2 相同：w2[seg343 * 16 + coeff]。
      * ============================================================ */
     uint64_t w2[7 * 7 * 7 * 16];
     memset(w2, 0, sizeof(w2));
 
     for (int seg49 = 0; seg49 < 49; seg49++)
     {
         /*
          * 在 aws2[seg49 * 7 * 16] 中，按 stride=4 布局存放了7个评估点，
          * 每评估点4个系数。即：
          *   评估点 k 的系数 c 存在 aws2[seg49*7*16 + k*4 + c]
          *
          * 这7组（k=0..6）需要分别做 product16（每组4个系数）。
          * 但注意：product16 的4个输入系数要从 aws2 中连续读出：
          *   a_sub[c] = aws2[seg49*7*16 + k*4 + c]，c=0..3
          *
          * 同理 b_sub，输出 w2_sub[0..6]（7系数）写回 w2 同位置。
          */
         for (int k = 0; k < 7; k++)
         {
             int seg343 = seg49 * 7 + k;
             const uint64_t *pa = &aws2[seg343 * 4];
             const uint64_t *pb = &bws2[seg343 * 4];
 
             /*
              * product16 输出7个系数存入 tmp，
              * 这7个系数将作为 interp_layer(N=16,stride=4) 的输入：
              *   w2[point * stride + i]，point=0..6，stride=4，i=k（此处 i=k 不对）
              *
              * 重新理解布局：
              * interp_layer(w, c, stride=4) 读取 w[point*4 + i]，i=0..3。
              * 对于 seg49 块，其插值输入应为：
              *   w_for_interp[point*4 + k] = product结果的 point 系数
              *   (k 是该 seg49 内的子块索引，即位置 i)
              *
              * 所以 product16 的输出 tmp[point] 应写入：
              *   w2[seg49*7*4*4 + point*4 + k]（如果按4×4块组织）
              *
              * 等价：w2_interp[seg49 * (7*4) + point*4 + k]
              * 即   w2[seg49 * 28 + point * 4 + k]
              */
             uint64_t tmp[7];
             product16(pa, pb, tmp);
 
             /* 将 product16 的7个输出写入插值输入布局 */
             for (int pt = 0; pt < 7; pt++)
                 w2[seg49 * 28 + pt * 4 + k] = tmp[pt];
         }
     }
 
     /* ============================================================
      * STAGE 3：三级插值（逆序）
      *
      * L2 插值：49 次 interp_layer(stride=4，N=16)
      *   输入 w2[seg49*28]（布局 [point*4+i]）→ 输出 w1_seg[0..15]
      *   合并进 w1[seg49*16]
      *
      * L1 插值：7 次 interp_layer(stride=16，N=64)
      *   输入 w1[i*7*16]（布局 [point*16+j]）→ 输出 w0_seg[0..63]
      *
      * L0 插值：1 次 interp_layer(stride=64，N=256)
      *   输入 w0[i*7*64]（布局 [point*64+j]）→ 输出 c[0..255]（累积）
      * ============================================================ */
 
     /* --- L2 插值：49 × interp_layer(stride=4) → w1[49×16] --- */
     uint64_t w1[7 * 7 * 16];
     memset(w1, 0, sizeof(w1));
 
     for (int seg49 = 0; seg49 < 49; seg49++)
     {
         interp_layer(&w2[seg49 * 28], &w1[seg49 * 16], /*stride=*/4);
         /* 结果 w1[seg49*16 + 0..15] 是16个系数的一个段 */
     }
 
     /*
      * 此时 w1 的布局：
      *   w1[(i*7+j)*16 + coeff]，i=L0点，j=L1点，coeff=0..15
      *
      * 需要重排为 interp_layer(stride=16) 所需的 [point*16+coeff] 布局：
      *   w1_for_L1[i * 7 * 16 + j * 16 + coeff] 已经是这个形式！
      *   （i*7+j 就是 seg49，乘以16加coeff）
      * 无需额外重排。
      */
 
     /* --- L1 插值：7 × interp_layer(stride=16) → w0[7×64] --- */
     uint64_t w0[7 * 64];
     memset(w0, 0, sizeof(w0));
 
     for (int i = 0; i < 7; i++)
     {
         interp_layer(&w1[i * 7 * 16], &w0[i * 64], /*stride=*/16);
     }
 
     /* --- L0 插值：1 × interp_layer(stride=64) → c[256] --- */
     memset(c, 0, 256 * sizeof(uint64_t));  /* 只写前256个 */
 
     /*
      * w0 布局：w0[i*64 + coeff]，i=L0点，coeff=0..63
      * interp_layer 期望 w[point*stride + i]，stride=64
      * 恰好匹配：w0[point*64 + i] ✓
      */
     interp_layer(w0, c, /*stride=*/64);
 
     /* --- 最终截断 --- */
     for (int i = 0; i < 256; i++)
         c[i] &= mask24;
 }