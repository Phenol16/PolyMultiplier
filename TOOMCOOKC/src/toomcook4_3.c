#include "poly.h"

#define mask39 0x7FFFFFFFFFULL
#define mask37 0x1FFFFFFFFFULL
#define mask36 0xFFFFFFFFFULL
#define mask34 0x3FFFFFFFFULL
#define mask33 0x1FFFFFFFFULL
#define mask31 0x7FFFFFFFULL
#define mask30 0x3FFFFFFFULL
#define mask28 0xFFFFFFFULL
#define mask27 0x7FFFFFFULL
#define mask25 0x1FFFFFFULL
#define mask24 0xFFFFFFULL

void eval_layer_tc43(const uint64_t *in, uint64_t *out, int seg_len)
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

typedef struct
{
    uint64_t mk;
    uint64_t mk_r2;
    uint64_t mk_r13;
    uint64_t inv3;
    uint64_t inv9;
    uint64_t inv18;
} InterpParamTC43;
const InterpParamTC43 INTERP_PARAMS_TC43[] = {
    {mask36, mask33, mask34, 0xAAAAAAABULL, 0x238E38E39ULL, 0x2EEEEEEEFULL},
    {mask33, mask30, mask31, 0x2AAAAAABULL, 0x38E38E39ULL, 0x6EEEEEEFULL},
    {mask30, mask27, mask28, 0x2AAAAABULL, 0x8E38E39ULL, 0xEEEEEEFULL},
    {mask27, mask24, mask25, 0xAAAAABULL, 0xE38E39ULL, 0xEEEEEFULL},
};
int interp_param_idx_tc43(int stride)
{
    switch (stride)
    {
    case 4:
        return 0;
    case 16:
        return 1;
    case 64:
        return 2;
    case 256:
        return 3;
    default:
        return -1;
    }
}
void interp_core_tc43(
    const uint64_t *w,
    uint64_t *c,
    int i,
    int stride,
    const InterpParamTC43 *p,
    uint64_t prev_r0,
    uint64_t prev_r1,
    uint64_t prev_r2,
    uint64_t *out_r0,
    uint64_t *out_r1,
    uint64_t *out_r2)
{
    uint64_t p0 = w[0 * stride + i];
    uint64_t p1 = w[1 * stride + i];
    uint64_t p2 = w[2 * stride + i];
    uint64_t p3 = w[3 * stride + i];
    uint64_t p4 = w[4 * stride + i];
    uint64_t p5 = w[5 * stride + i];
    uint64_t p6 = w[6 * stride + i];

    uint64_t mk = p->mk;
    uint64_t mk2 = p->mk_r2;
    uint64_t mk3 = p->mk_r13;

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

    *out_r0 = p0;
    *out_r1 = r1;
    *out_r2 = r2;
}
void interp_layer_tc43(const uint64_t *w, uint64_t *c, int stride)
{
    int idx = interp_param_idx_tc43(stride);
    const InterpParamTC43 *p = &INTERP_PARAMS_TC43[idx];

    uint64_t pr0 = 0, pr1 = 0, pr2 = 0;
    uint64_t nr0, nr1, nr2;

    for (int i = 0; i < stride; i++)
    {
        interp_core_tc43(w, c, i, stride, p, pr0, pr1, pr2, &nr0, &nr1, &nr2);
        pr0 = nr0;
        pr1 = nr1;
        pr2 = nr2;
    }

    c[0] -= pr2;
    c[1] -= pr1;
    c[2] -= pr0;
}
void product4_tc43(const uint64_t *a, const uint64_t *b, uint64_t *c)
{
    uint64_t aws[7], bws[7];
    eval_layer_tc43(a, aws, 4);
    eval_layer_tc43(b, bws, 4);

    uint64_t w[7];
    for (int i = 0; i < 7; i++)
    {
        uint64_t aw = aws[i] & mask39;
        uint64_t bw = bws[i] & 0x1FFFFFFFULL;
        uint64_t bw_sign = bw & 0x10000000ULL;
        int64_t bw_s = (int64_t)((bw | (uint64_t)(-(int64_t)bw_sign)) & mask39);
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
    c[1] = (r5 - r1) & mask36;
    c[2] = (r4 - w[0]) & mask36;
    c[3] = r3 & mask36;
    c[4] = 0;
    c[5] = 0;
    c[6] = 0;
}
void core16_tc43(const uint64_t *a16, const uint64_t *b16, uint64_t *c16)
{
    uint64_t ae[7 * 4], be[7 * 4], w[7 * 4];

    eval_layer_tc43(a16, ae, 16);
    eval_layer_tc43(b16, be, 16);

    for (int pt = 0; pt < 7; pt++)
    {
        uint64_t tmp[7];
        product4_tc43(&ae[pt * 4], &be[pt * 4], tmp);
        for (int k = 0; k < 4; k++)
        {
            w[pt * 4 + k] = tmp[k];
        }
    }

    interp_layer_tc43(w, c16, 4);
}

uint64_t tc4_eval_point(uint64_t r0, uint64_t r1, uint64_t r2, uint64_t r3, int point_idx)
{
    switch (point_idx)
    {
    case 0:
        return r3;
    case 1:
        return (r3 << 3) + (r2 << 2) + (r1 << 1) + r0;
    case 2:
        return r0 + r1 + r2 + r3;
    case 3:
        return (r0 + r2) - (r1 + r3);
    case 4:
        return (((r0 << 2) + r2) << 1) + ((r1 << 2) + r3);
    case 5:
        return (((r0 << 2) + r2) << 1) - ((r1 << 2) + r3);
    case 6:
        return r0;
    default:
        return 0;
    }
}

uint64_t load_coeff_4d(const uint64_t *in, int i, int j, int k, int l)
{
    return in[64 * l + 16 * k + 4 * j + i];
}

void build_eval_vector16_striding(
    const uint64_t *in,
    int pt0,
    int pt1,
    int pt2,
    uint64_t out_vec16[16])
{
    for (int l = 0; l < 16; l++)
    {
        uint64_t lv2[4];

        for (int k = 0; k < 4; k++)
        {
            uint64_t lv1[4];

            for (int j = 0; j < 4; j++)
            {
                uint64_t r0 = load_coeff_4d(in, 0, j, k, l);
                uint64_t r1 = load_coeff_4d(in, 1, j, k, l);
                uint64_t r2 = load_coeff_4d(in, 2, j, k, l);
                uint64_t r3 = load_coeff_4d(in, 3, j, k, l);
                lv1[j] = tc4_eval_point(r0, r1, r2, r3, pt0);
            }

            lv2[k] = tc4_eval_point(lv1[0], lv1[1], lv1[2], lv1[3], pt1);
        }

        out_vec16[l] = tc4_eval_point(lv2[0], lv2[1], lv2[2], lv2[3], pt2);
    }
}

void toomcook4_3(const uint64_t *a, const uint64_t *b, uint64_t *c)
{
    uint64_t w2[7 * 7 * 7 * 16];

    uint64_t avec_buf[2][16];
    uint64_t bvec_buf[2][16];
    uint64_t prod_buf[2][16];

    int p0 = 0, p1 = 0, p2 = 0;
    build_eval_vector16_striding(a, p0, p1, p2, avec_buf[0]);
    build_eval_vector16_striding(b, p0, p1, p2, bvec_buf[0]);

    for (int seg343 = 0; seg343 < 343; seg343++)
    {
        int cur = seg343 & 1;
        int nxt = cur ^ 1;

        core16_tc43(avec_buf[cur], bvec_buf[cur], prod_buf[cur]);
        for (int t = 0; t < 16; t++)
        {
            w2[seg343 * 16 + t] = prod_buf[cur][t];
        }

        if (seg343 + 1 < 343)
        {
            int nxt_seg = seg343 + 1;
            int npt0 = nxt_seg / 49;
            int npt1 = (nxt_seg / 7) % 7;
            int npt2 = nxt_seg % 7;

            build_eval_vector16_striding(a, npt0, npt1, npt2, avec_buf[nxt]);
            build_eval_vector16_striding(b, npt0, npt1, npt2, bvec_buf[nxt]);
        }
    }

    uint64_t w1[7 * 7 * 64];
    for (int seg49 = 0; seg49 < 49; seg49++)
    {
        interp_layer_tc43(&w2[seg49 * 7 * 16], &w1[seg49 * 64], 16);
    }

    uint64_t w0[7 * 256];
    for (int i = 0; i < 7; i++)
    {
        interp_layer_tc43(&w1[i * 7 * 64], &w0[i * 256], 64);
    }
    interp_layer_tc43(w0, c, 256);

    for (int i = 0; i < 1024; i++)
    {
        c[i] &= mask24;
    }
}