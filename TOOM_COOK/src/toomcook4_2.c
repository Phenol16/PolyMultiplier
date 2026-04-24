#include "poly.h"

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

void eval_layer(const uint64_t *in, uint64_t *out, int seg_len)
{
    int stride = seg_len / 4;

    for (int j = 0; j < stride; j++)
    {
        uint64_t r0 = in[4 * j + 0];
        uint64_t r1 = in[4 * j + 1];
        uint64_t r2 = in[4 * j + 2];
        uint64_t r3 = in[4 * j + 3];
        uint64_t t0, t1;

        out[0 * stride + j] = r3;
        out[1 * stride + j] = (r3 << 3) + (r2 << 2) + (r1 << 1) + r0;

        t0 = r0 + r2;
        t1 = r1 + r3;
        out[2 * stride + j] = t0 + t1;
        out[3 * stride + j] = t0 - t1;

        t0 = (((r0 << 2) + r2) << 1);
        t1 = (r1 << 2) + r3;
        out[4 * stride + j] = t0 + t1;
        out[5 * stride + j] = t0 - t1;
        out[6 * stride + j] = r0;
    }
}

 typedef struct {
     uint64_t mk;      /* 主掩码 */
     uint64_t mk_r2;   /* r2 输出掩码 */
     uint64_t mk_r13;  /* r1/r3 输出掩码 */
     uint64_t inv3;    /* 乘以 1/3 的魔数 */
     uint64_t inv9;    /* 乘以 1/9 的魔数 */
     uint64_t inv18;   /* 乘以 1/18 的魔数 */
 } InterpParam;
 
const InterpParam INTERP_PARAMS[] = {
     /* N=4   */ { mask36, mask33, mask34, 0xAAAAAAABULL,  0x238E38E39ULL, 0x2EEEEEEEFULL },
     /* N=16  */ { mask33, mask30, mask31, 0x2AAAAAABULL,  0x38E38E39ULL,  0x6EEEEEEFULL  },
     /* N=64  */ { mask30, mask27, mask28, 0x2AAAAABULL,   0x8E38E39ULL,   0xEEEEEEFULL   },
     /* N=256 */ { mask27, mask24, mask25, 0xAAAAABULL,    0xE38E39ULL,    0xEEEEEFULL    },
 };

int interp_param_idx(int stride)
{
    switch (stride) {
        case   4: return 0;
        case  16: return 1;
        case  64: return 2;
        case 256: return 3;
         default:  return -1;
     }
 }
 
void interp_core(
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
 
     r6 = p6;

     c[4 * i + 3] = r3;
     c[4 * i + 0] = r6 + prev_r2;
     c[4 * i + 1] = r5 + prev_r1;
     c[4 * i + 2] = r4 + prev_r0;
 
     /* 传出本次高位系数给下一块 */
     *out_r0 = p0;
     *out_r1 = r1;
     *out_r2 = r2;
 }
 


void interp_layer(const uint64_t *w, uint64_t *c, int stride)
{
    int idx = interp_param_idx(stride);
    const InterpParam *p = &INTERP_PARAMS[idx];

    uint64_t pr0 = 0, pr1 = 0, pr2 = 0;
    uint64_t nr0, nr1, nr2;

    for (int i = 0; i < stride; i++)
    {
        interp_core(w, c, i, stride, p,
                    pr0, pr1, pr2,
                    &nr0, &nr1, &nr2);
        pr0 = nr0; pr1 = nr1; pr2 = nr2;
    }

    c[0] -= pr2;
    c[1] -= pr1;
    c[2] -= pr0;
}

void product4(const uint64_t *a, const uint64_t *b, uint64_t *c)
 {
     uint64_t aws[7], bws[7];
     eval_layer(a,aws,4);
     eval_layer(b,bws,4);
     uint64_t w[7];
     for (int i = 0; i < 7; i++)
     {
         uint64_t aw      = aws[i] & mask39;
         uint64_t bw      = bws[i] & 0x1FFFFFFFULL;
         uint64_t bw_sign = bw & 0x10000000ULL;
         int64_t  bw_s    = (int64_t)((bw | (uint64_t)(-(int64_t)bw_sign)) & mask39);
         w[i] = (uint64_t)((int64_t)aw * bw_s) & mask39;
     }
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
 
     c[0] = (w[6] - r2) & mask36;
     c[1] = (r5 - r1)   & mask36;
     c[2] = (r4 - w[0]) & mask36;
     c[3] = r3          & mask36;
     c[4] = 0;
     c[5] = 0;
     c[6] = 0;
 }
 
void core16(const uint64_t *a16, const uint64_t *b16, uint64_t *c16)
{
    uint64_t ae[7 * 4], be[7 * 4], w[7 * 4];

    eval_layer(a16, ae, 16);
    eval_layer(b16, be, 16);

    for (int pt = 0; pt < 7; pt++)
    {
        uint64_t tmp[7];
        product4(&ae[pt * 4], &be[pt * 4], tmp);
        for (int k = 0; k < 4; k++)
            w[pt * 4 + k] = tmp[k];
    }

    interp_layer(w, c16, /*stride=*/4);
}


 /* ================================================================== */
 /*  主函数                             */
 /* ================================================================== */
void toomcook4_2(const uint64_t *a, const uint64_t *b, uint64_t *c)
{
    /* L0/L1/L2 评估：1024 -> 7x256 -> 49x64 -> 343x16 */
    uint64_t aws0[7 * 256], bws0[7 * 256];
    eval_layer(a, aws0, 1024);
    eval_layer(b, bws0, 1024);

     uint64_t aws1[7 * 7 * 64], bws1[7 * 7 * 64];
     for (int i = 0; i < 7; i++)
     {
         eval_layer(&aws0[i * 256], &aws1[i * 7 * 64], 256);
         eval_layer(&bws0[i * 256], &bws1[i * 7 * 64], 256);
     }

     uint64_t aws2[7 * 7 * 7 * 16], bws2[7 * 7 * 7 * 16];
     for (int i = 0; i < 7; i++)
     for (int j = 0; j < 7; j++)
     {
         int seg49 = i * 7 + j;
         eval_layer(&aws1[seg49 * 64], &aws2[seg49 * 7 * 16], 64);
         eval_layer(&bws1[seg49 * 64], &bws2[seg49 * 7 * 16], 64);
     }

    /* 343 次 N=16 */
    uint64_t w2[7 * 7 * 7 * 16];
    for (int seg343 = 0; seg343 < 343; seg343++)
        core16(&aws2[seg343 * 16], &bws2[seg343 * 16], &w2[seg343 * 16]);

    /* 插值：343x16 -> 49x64 -> 7x256 -> 1024 */
    uint64_t w1[7 * 7 * 64];
    for (int seg49 = 0; seg49 < 49; seg49++)
        interp_layer(&w2[seg49 * 7 * 16], &w1[seg49 * 64], /*stride=*/16);

    uint64_t w0[7 * 256];
    for (int i = 0; i < 7; i++)
        interp_layer(&w1[i * 7 * 64], &w0[i * 256], /*stride=*/64);
    interp_layer(w0, c, /*stride=*/256);

    for (int i = 0; i < 1024; i++)
        c[i] &= mask24;
}